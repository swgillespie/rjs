//! An activation record is a runtime representation of a lexical environment.
//! Since ECMAScript mandates nominative variable lookups for correctness
//! in `with` statements, we are required to (at least some of the time)
//! construct an activation chain for every function call.
//!
//! Activations are also "captured" by closures, which allows closures to
//! reference their environment even if the closure outlives the activation's
//! original lifetime. For this reason, activations are *heap-allocated* and
//! are managed by the garbage collector.
//!
//! This type loosely corresponds to the Environment Record specification type,
//! in section 10.2.1 of the ECMAScript 5.1 spec. This implementation diverges
//! from the spec's implementation in that an Activation has a reference to its parent
//! Activation and uses it to perform correct identifier resolution throughout the
//! entire scope chain and not just that one activation.

use compiler::InternedString;
use heap::{Trace, HeapObject, ActivationPtr, ToHeapObject};
use std::default::Default;
use std::vec::IntoIter;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use super::{Value, RootedValue};
use values::object::HostObject;
use values::property::Property;
use values::{EvalResult, EvalValue};
use exec::engine::ExecutionEngine;
use heap::RootedActivationPtr;

/// An activation represents a lexical environment at runtime. It maintains a mapping
/// of identifier names to runtime values and is queried by the interpreter to resolve
/// identifier references. It also maintains the value of `this` in the current lexical
/// environment.
pub struct Activation {
    /// Each activation has some backing representation depending on what type of
    /// activation it is.
    backing_repr: ActivationKind,
}

impl Trace for Activation {
    fn trace(&self) -> IntoIter<HeapObject> {
        match self.backing_repr {
            ActivationKind::Function(ref act) => act.trace(),
            ActivationKind::With(ref act) => act.trace(),
            ActivationKind::DefaultSentinel => self.panic_on_default_sentinel(),
        }
    }
}

impl Default for Activation {
    fn default() -> Activation {
        Activation { backing_repr: ActivationKind::DefaultSentinel }
    }
}

impl Activation {
    pub fn new_function_activation(ee: &mut ExecutionEngine,
                                   parent: Option<ActivationPtr>,
                                   this: RootedValue)
                                   -> RootedActivationPtr {
        let act = Activation {
            backing_repr: ActivationKind::Function(FunctionActivation::new(parent, this)),
        };
        let heap_ptr = ee.heap_mut().allocate_activation();
        *heap_ptr.borrow_mut() = act;
        heap_ptr
    }

    pub fn new_object_activation(ee: &mut ExecutionEngine,
                                 parent: ActivationPtr,
                                 value: RootedValue)
                                 -> RootedActivationPtr {
        let act = Activation {
            backing_repr: ActivationKind::With(WithActivation::new(parent, value)),
        };
        let heap_ptr = ee.heap_mut().allocate_activation();
        *heap_ptr.borrow_mut() = act;
        heap_ptr
    }

    pub fn has_binding(&self, ee: &mut ExecutionEngine, ident: InternedString) -> bool {
        match self.backing_repr {
            ActivationKind::Function(ref act) => act.has_binding(ee, ident),
            ActivationKind::With(ref act) => act.has_binding(ee, ident),
            ActivationKind::DefaultSentinel => self.panic_on_default_sentinel(),
        }
    }

    pub fn create_mutable_binding(&mut self,
                                  ee: &mut ExecutionEngine,
                                  ident: InternedString,
                                  deletable: bool)
                                  -> EvalResult<()> {
        match self.backing_repr {
            ActivationKind::Function(ref mut act) => {
                act.create_mutable_binding(ee, ident, deletable)
            }
            ActivationKind::With(ref mut act) => act.create_mutable_binding(ee, ident, deletable),
            ActivationKind::DefaultSentinel => self.panic_on_default_sentinel(),
        }
    }

    pub fn set_mutable_binding(&mut self,
                               ee: &mut ExecutionEngine,
                               ident: InternedString,
                               value: &RootedValue,
                               should_throw: bool)
                               -> EvalResult<()> {
        match self.backing_repr {
            ActivationKind::Function(ref mut act) => {
                act.set_mutable_binding(ee, ident, value, should_throw)
            }
            ActivationKind::With(ref mut act) => {
                act.set_mutable_binding(ee, ident, value, should_throw)
            }
            ActivationKind::DefaultSentinel => self.panic_on_default_sentinel(),
        }
    }

    pub fn get_binding_value(&self, ee: &mut ExecutionEngine, ident: InternedString) -> EvalValue {
        match self.backing_repr {
            ActivationKind::Function(ref act) => act.get_binding_value(ee, ident),
            ActivationKind::With(ref act) => act.get_binding_value(ee, ident),
            ActivationKind::DefaultSentinel => self.panic_on_default_sentinel(),
        }
    }

    pub fn delete_binding(&mut self,
                          ee: &mut ExecutionEngine,
                          ident: InternedString)
                          -> EvalResult<bool> {
        match self.backing_repr {
            ActivationKind::Function(ref mut act) => act.delete_binding(ee, ident),
            ActivationKind::With(ref mut act) => act.delete_binding(ee, ident),
            ActivationKind::DefaultSentinel => self.panic_on_default_sentinel(),
        }
    }

    pub fn implicit_this_value(&self, ee: &mut ExecutionEngine) -> RootedValue {
        match self.backing_repr {
            ActivationKind::Function(ref act) => act.implicit_this_value(ee),
            ActivationKind::With(ref act) => act.implicit_this_value(ee),
            ActivationKind::DefaultSentinel => self.panic_on_default_sentinel(),
        }
    }

    // This should never actually happen, but Rust requires us to account for the possibility
    // anyway.
    #[cold]
    #[inline(never)]
    fn panic_on_default_sentinel(&self) -> ! {
        panic!("failed to initialize activation properly before use");
    }
}

pub enum GetBindingResult {
    Success(Value),
    ReadOfUninitializedBinding,
    NotFound,
}

/// An activation can be either a FunctionActivation, which is created at the start
/// of a function call, or a WithActivation, which is created by a `with` statement.
/// DefaultSentinel is used in the Activation implementation of Default, which is
/// required in order to be allocated on the managed heap.
enum ActivationKind {
    Function(FunctionActivation),
    With(WithActivation),
    DefaultSentinel,
}

struct ActivationEntry {
    deletable: bool,
    mutable: bool,
    has_been_initialized: bool,
    value: Value,
}

impl ActivationEntry {
    pub fn new(deletable: bool, mutable: bool) -> ActivationEntry {
        ActivationEntry {
            deletable: deletable,
            mutable: mutable,
            has_been_initialized: false,
            value: Value::undefined(),
        }
    }

    pub fn is_deletable(&self) -> bool {
        self.deletable
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn value(&self) -> Value {
        self.value
    }

    pub fn set_value(&mut self, value: Value) {
        debug_assert!(self.is_mutable());
        self.value = value;
    }

    pub fn has_been_initialized(&self) -> bool {
        debug_assert!(!self.is_mutable());
        self.has_been_initialized
    }

    #[allow(dead_code)]
    pub fn mark_as_initialized(&mut self) {
        debug_assert!(!self.is_mutable());
        self.has_been_initialized = true;
    }
}

/// A function activation is simply a hashmap of string values to ECMAScript values.
struct FunctionActivation {
    parent: Option<ActivationPtr>,
    this: Value,
    map: HashMap<InternedString, ActivationEntry>,
}

impl Trace for FunctionActivation {
    fn trace(&self) -> IntoIter<HeapObject> {
        let mut pointers = vec![];
        if let Some(value) = self.parent {
            if let Some(ptr) = value.to_heap_object() {
                pointers.push(ptr);
            }
        }

        if let Some(ptr) = self.this.to_heap_object() {
            pointers.push(ptr);
        }

        for val in self.map.values() {
            if let Some(ptr) = val.value.to_heap_object() {
                pointers.push(ptr)
            }
        }

        pointers.into_iter()
    }
}

impl FunctionActivation {
    pub fn new(parent: Option<ActivationPtr>, this: RootedValue) -> FunctionActivation {
        FunctionActivation {
            parent: parent,
            this: this.into_inner(),
            map: HashMap::new(),
        }
    }

    pub fn has_binding(&self, ee: &mut ExecutionEngine, ident: InternedString) -> bool {
        if self.map.contains_key(&ident) {
            return true;
        }

        if let Some(activation) = self.parent {
            // an invariant of the activation tree is that there are no cycles,
            // so no one activation will be borrowed more than once.
            activation.borrow().has_binding(ee, ident)
        } else {
            false
        }
    }

    pub fn create_mutable_binding(&mut self,
                                  ee: &mut ExecutionEngine,
                                  ident: InternedString,
                                  deletable: bool)
                                  -> EvalResult<()> {
        debug_assert!(!self.map.contains_key(&ident));
        debug!(target: "activation", "creating activation binding for \"{}\"", ee.interner().get(ident));
        let entry = ActivationEntry::new(deletable, true);
        self.map.insert(ident, entry);
        return Ok(());
    }

    pub fn set_mutable_binding(&mut self,
                               ee: &mut ExecutionEngine,
                               ident: InternedString,
                               value: &RootedValue,
                               should_throw: bool)
                               -> EvalResult<()> {
        debug!(target: "activation", "setting mutable binding for \"{}\"", ee.interner().get(ident));
        match self.map.entry(ident) {
            Entry::Occupied(mut entry) => {
                if !entry.get().is_mutable() {
                    if should_throw {
                        return ee.throw_type_error("set_mutable_binding");
                    }

                    return Ok(());
                }

                entry.get_mut().set_value(value.clone().into_inner());
                return Ok(());
            }
            Entry::Vacant(_) => {
                panic!("set_mutable_binding called on unbound identifier");
            }
        }
    }

    pub fn get_binding_value(&self, ee: &mut ExecutionEngine, ident: InternedString) -> EvalValue {
        debug!(target: "activation", "getting binding value for \"{}\"", ee.interner().get(ident));
        if let Some(entry) = self.map.get(&ident) {
            debug!(target: "activation", "activation hit for \"{}\"", ee.interner().get(ident));
            if !entry.is_mutable() && !entry.has_been_initialized() {
                return ee.throw_reference_error("get_binding_value");
            }

            return Ok(ee.heap_mut().root_value(entry.value()));
        }

        debug!(target: "activation", "activation miss for \"{}\", searching parent activation", ee.interner().get(ident));
        if let Some(activation) = self.parent {
            activation.borrow().get_binding_value(ee, ident)
        } else {
            return ee.throw_reference_error("get_binding_value");
        }
    }

    pub fn delete_binding(&mut self,
                          _: &mut ExecutionEngine,
                          ident: InternedString)
                          -> EvalResult<bool> {
        match self.map.entry(ident) {
            Entry::Occupied(entry) => {
                if !entry.get().is_deletable() {
                    return Ok(false);
                }

                entry.remove();
                return Ok(true);
            }
            Entry::Vacant(_) => return Ok(true),
        }
    }

    pub fn implicit_this_value(&self, ee: &mut ExecutionEngine) -> RootedValue {
        ee.heap_mut().root_value(self.this)
    }

    #[allow(dead_code)]
    pub fn create_immutable_binding(&mut self, ident: InternedString) {
        debug_assert!(!self.map.contains_key(&ident));
        // SPEC_NOTE: can an immutable binding be deleted? going with no for now. the spec
        // doesn't say.
        let entry = ActivationEntry::new(false, false);
        self.map.insert(ident, entry);
    }

    #[allow(dead_code)]
    pub fn initialize_immutable_binding(&mut self, ident: InternedString, value: &RootedValue) {
        let binding = self.map
                          .get_mut(&ident)
                          .expect("initialize_immutable_binding called on unbound identifier");
        debug_assert!(!binding.is_mutable() && !binding.has_been_initialized());
        binding.set_value(value.clone().into_inner());
        binding.mark_as_initialized();
    }
}

/// A with activation wraps an object and proxies all identifier lookups through
/// the given object's properties.
struct WithActivation {
    parent: Option<ActivationPtr>,
    object: Value,
    provide_this: bool,
}

impl Trace for WithActivation {
    fn trace(&self) -> IntoIter<HeapObject> {
        let mut pointers = vec![];
        if let Some(value) = self.parent {
            if let Some(ptr) = value.to_heap_object() {
                pointers.push(ptr);
            }
        }

        if let Some(ptr) = self.object.to_heap_object() {
            pointers.push(ptr);
        }

        pointers.into_iter()
    }
}

impl WithActivation {
    pub fn new(parent: ActivationPtr, value: RootedValue) -> WithActivation {
        WithActivation {
            parent: Some(parent),
            object: value.into_inner(),
            provide_this: false,
        }
    }

    #[allow(dead_code)]
    pub fn provide_this(&mut self) {
        self.provide_this = true;
    }

    pub fn has_binding(&self, ee: &mut ExecutionEngine, ident: InternedString) -> bool {
        // this is a little awkward due to rust-lang/rust#22323.
        let obj = self.object.unwrap_object();
        return obj.borrow_mut().has_property(ee, ident);
    }

    pub fn create_mutable_binding(&mut self,
                                  ee: &mut ExecutionEngine,
                                  ident: InternedString,
                                  deletable: bool)
                                  -> EvalResult<()> {
        let obj = self.object.unwrap_object();
        debug_assert!(!obj.borrow_mut().has_property(ee, ident));
        let prop = Property::new_data_descriptor(true, true, deletable);
        return obj.borrow_mut().define_own_property(ee, ident, prop, true).map(|_| ());
    }

    pub fn set_mutable_binding(&mut self,
                               ee: &mut ExecutionEngine,
                               ident: InternedString,
                               value: &RootedValue,
                               should_throw: bool)
                               -> EvalResult<()> {
        let obj = self.object.unwrap_object();
        return obj.borrow_mut().put(ee, ident, value, should_throw);
    }

    pub fn get_binding_value(&self, ee: &mut ExecutionEngine, ident: InternedString) -> EvalValue {
        let obj = self.object.unwrap_object();
        let mut cell = obj.borrow_mut();
        if !cell.has_property(ee, ident) {
            return ee.throw_type_error("get_binding_value");
        }

        return cell.get(ee, ident);
    }

    pub fn delete_binding(&mut self,
                          ee: &mut ExecutionEngine,
                          ident: InternedString)
                          -> EvalResult<bool> {
        let obj = self.object.unwrap_object();
        return obj.borrow_mut().delete(ee, ident, false);
    }

    pub fn implicit_this_value(&self, ee: &mut ExecutionEngine) -> RootedValue {
        if self.provide_this {
            ee.heap_mut().root_value(self.object)
        } else {
            ee.heap_mut().root_value(Value::undefined())
        }
    }
}
