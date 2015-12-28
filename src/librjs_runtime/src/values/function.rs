use compiler::{CompiledFunction, InternedString};
use values::property::Property;
use values::object::{self, PropertyMap, HostObject, Object};
use values::{RootedValue, EvalResult, EvalValue, Value, IntoRootedValue};
use heap::{Trace, HeapObject, ToHeapObject, ActivationPtr, RootedActivationPtr};
use exec::engine::ExecutionEngine;

use std::vec::IntoIter;
use std::collections::HashMap;

pub enum FunctionType {
    Interpreted(CompiledFunction),
    Native(Box<Fn(&mut ExecutionEngine, RootedValue, Vec<RootedValue>) -> EvalValue>),
}

pub struct Function {
    code: FunctionType,
    length: Property,
    // target_function: Property,
    // bound_this: Property,
    // bound_arguments: Property,
    scope: ActivationPtr,
    other_props: HashMap<InternedString, Property>,
}

impl Function {
    // Section 13.2 - Creating Function Objects
    pub fn new(ee: &mut ExecutionEngine,
               code: FunctionType,
               length: usize,
               scope: &RootedActivationPtr)
               -> RootedValue {
        // TODO function prototype
        let obj = ee.heap_mut().allocate_object();
        let len = ee.heap_mut().allocate_number();
        *len.borrow_mut() = length as f64;
        let func = Function {
            code: code,
            length: Property::Named {
                value: Some(Value::Number(len.into_inner())),
                writable: Some(false),
                enumerable: Some(false),
                configurable: Some(false),
            },
            scope: scope.clone().into_inner(),
            other_props: HashMap::new(),
        };

        // TODO contructor and prototype
        *obj.borrow_mut() = Object::Function(func);
        obj.into_rooted_value(ee.heap_mut())
    }
}

impl Trace for Function {
    fn trace(&self) -> IntoIter<HeapObject> {
        let mut ptrs = vec![];
        ptrs.extend(self.length.trace());
        // ptrs.extend(self.target_function.trace());
        // ptrs.extend(self.bound_this.trace());
        // ptrs.extend(self.bound_arguments.trace());
        ptrs.push(self.scope.to_heap_object().expect("activations are always heap objects"));
        ptrs.into_iter()
    }
}

impl PropertyMap for Function {
    fn get_raw_property(&self,
                        ee: &mut ExecutionEngine,
                        name: InternedString)
                        -> Option<&Property> {
        match ee.interner().get(name) {
            "length" => Some(&self.length),
            _ => self.other_props.get(&name),
        }
    }

    fn get_raw_property_mut(&mut self,
                            ee: &mut ExecutionEngine,
                            name: InternedString)
                            -> Option<&mut Property> {
        match ee.interner().get(name) {
            "length" => Some(&mut self.length),
            _ => self.other_props.get_mut(&name),
        }
    }

    fn remove_property(&mut self, _: &mut ExecutionEngine, name: InternedString) {
        // assume that the implementation of Delete has already validated that this
        // delete is valid. This means that none of the built-in properties will get
        // deleted by this method.
        debug_assert!(self.other_props.contains_key(&name));
        self.other_props.remove(&name);
    }

    fn add_property(&mut self, _: &mut ExecutionEngine, name: InternedString, prop: Property) {
        self.other_props.insert(name, prop);
    }
}

impl HostObject for Function {
    /// [[GetOwnProperty]], Section 8.12.1
    fn get_own_property(&mut self,
                        ee: &mut ExecutionEngine,
                        name: InternedString)
                        -> Option<Property> {
        object::object_get_own_property(self, ee, name)
    }

    /// [[GetProperty]], Section 8.12.2
    fn get_property(&mut self, ee: &mut ExecutionEngine, name: InternedString) -> Option<Property> {
        object::object_get_property(self, ee, name)
    }

    /// [[Class]], Section 8.6.2, Table 8
    fn class(&mut self, _: &mut ExecutionEngine) -> &'static str {
        "function"
    }

    /// [[Extensible]], Section 8.6.2, Table 8
    fn is_extensible(&mut self, _: &mut ExecutionEngine) -> bool {
        object::object_is_extensible()
    }

    /// [[Get]] for function objects, section 15.3.5.3
    fn get(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> EvalValue {
        // TODO objects created by bind?
        let result = try!(object::object_get(self, ee, property_name));
        if let FunctionType::Interpreted(ref code) = self.code {
            if code.is_strict() && ee.interner().get(property_name) == "caller" {
                return ee.throw_type_error("caller is not permitted in strict code");
            }
        }

        return Ok(result);
    }

    /// [[CanPut]], Section 8.12.4
    fn can_put(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> bool {
        object::object_can_put(self, ee, property_name)
    }

    fn put(&mut self,
           ee: &mut ExecutionEngine,
           property_name: InternedString,
           value: &RootedValue,
           should_throw: bool)
           -> EvalResult<()> {
        object::object_put(self, ee, property_name, value, should_throw)
    }

    fn has_property(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> bool {
        object::object_has_property(self, ee, property_name)
    }

    fn delete(&mut self,
              ee: &mut ExecutionEngine,
              property_name: InternedString,
              should_throw: bool)
              -> EvalResult<bool> {
        object::object_delete(self, ee, property_name, should_throw)
    }

    fn default_value(&mut self, ee: &mut ExecutionEngine, hint: &RootedValue) -> RootedValue {
        object::object_default_value(self, ee, hint)
    }

    /// [[DefineOwnProperty]], section 8.12.9
    fn define_own_property(&mut self,
                           ee: &mut ExecutionEngine,
                           name: InternedString,
                           prop: Property,
                           should_throw: bool)
                           -> EvalResult<bool> {
        object::object_define_own_property(self, ee, name, prop, should_throw)
    }

    fn call(&self,
            ee: &mut ExecutionEngine,
            args: Vec<RootedValue>,
            this: RootedValue)
            -> EvalValue {
        match self.code {
            FunctionType::Interpreted(ref code) => {
                // if this is a ECMAScript function, call it by invoking
                // the execution engine (which sets up the frame, scope, etc.)
                let scope = ee.heap_mut().root_value(self.scope);
                ee.call(code, &args, &this, scope)
            }
            FunctionType::Native(ref closure) => {
                // otherwise, this is a call into Rust code.
                // Rust code is free to do arbitrary things with the evaluation engine,
                // so we don't create a new scope. (In particular, eval is implemented
                // as a native function.)
                closure(ee, this, args)
            }
        }
    }

    fn construct(&mut self, _: &mut ExecutionEngine, _: Vec<RootedValue>) -> EvalValue {
        // TODO construct
        unimplemented!()
    }
}
