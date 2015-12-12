//! Objects are the cornerstone of ECMAScript and are the key to its versatility. Objects
//! themselves can be made of many things:
//! * `StandardObjects`, or regular objects as created by object literals,
//! * `FunctionObjects`, or function objects that are created from function literals or
//!    function declarations,
//! * Others which have yet to be written.

use super::super::heap::{Trace, HeapObject, ToHeapObject};
use super::super::compiler::InternedString;
use super::super::exec::engine::ExecutionEngine;
use super::{Value, RootedValue, EvalValue, EvalResult};

use std::vec::IntoIter;
use std::default::Default;
use std::collections::HashMap;

pub enum Object {
    Standard(StandardObject),
    HostObject(Box<HostObject>),
}

impl Default for Object {
    fn default() -> Object {
        // TODO
        Object::Standard(Default::default())
    }
}

impl Trace for Object {
    fn trace(&self) -> IntoIter<HeapObject> {
        match *self {
            Object::Standard(ref stdobj) => stdobj.trace(),
            Object::HostObject(ref hostobj) => hostobj.trace(),
        }
    }
}

impl HostObject for Object {
    fn get_prototype(&mut self, ee: &mut ExecutionEngine) -> RootedValue {
        match *self {
            Object::Standard(ref mut stdobj) => stdobj.get_prototype(ee),
            Object::HostObject(ref mut hostobj) => hostobj.get_prototype(ee),
        }
    }

    fn set_prototype(&mut self, ee: &mut ExecutionEngine, value: &RootedValue) {
        match *self {
            Object::Standard(ref mut stdobj) => stdobj.set_prototype(ee, value),
            Object::HostObject(ref mut hostobj) => hostobj.set_prototype(ee, value),
        }
    }

    fn class(&mut self, ee: &mut ExecutionEngine) -> &'static str {
        match *self {
            Object::Standard(ref mut stdobj) => stdobj.class(ee),
            Object::HostObject(ref mut hostobj) => hostobj.class(ee),
        }
    }

    fn is_extensible(&mut self, ee: &mut ExecutionEngine) -> bool {
        match *self {
            Object::Standard(ref mut stdobj) => stdobj.is_extensible(ee),
            Object::HostObject(ref mut hostobj) => hostobj.is_extensible(ee),
        }
    }

    fn get(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> EvalValue {
        match *self {
            Object::Standard(ref mut stdobj) => stdobj.get(ee, property_name),
            Object::HostObject(ref mut hostobj) => hostobj.get(ee, property_name),
        }
    }

    fn get_own_property(&mut self,
                        ee: &mut ExecutionEngine,
                        property_name: InternedString)
                        -> Option<Property> {
        match *self {
            Object::Standard(ref mut stdobj) => stdobj.get_own_property(ee, property_name),
            Object::HostObject(ref mut hostobj) => hostobj.get_own_property(ee, property_name),
        }
    }

    fn get_property(&mut self, ee: &mut ExecutionEngine, name: InternedString) -> Option<Property> {
        match *self {
            Object::Standard(ref mut stdobj) => stdobj.get_property(ee, name),
            Object::HostObject(ref mut hostobj) => hostobj.get_property(ee, name),
        }
    }

    fn put(&mut self,
           ee: &mut ExecutionEngine,
           property_name: InternedString,
           value: &RootedValue,
           should_throw: bool)
           -> EvalResult<()> {

        match *self {
            Object::Standard(ref mut stdobj) => stdobj.put(ee, property_name, value, should_throw),
            Object::HostObject(ref mut hostobj) => {
                hostobj.put(ee, property_name, value, should_throw)
            }
        }
    }

    fn can_put(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> bool {
        match *self {
            Object::Standard(ref mut stdobj) => stdobj.can_put(ee, property_name),
            Object::HostObject(ref mut hostobj) => hostobj.can_put(ee, property_name),
        }
    }

    fn has_property(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> bool {
        match *self {
            Object::Standard(ref mut stdobj) => stdobj.has_property(ee, property_name),
            Object::HostObject(ref mut hostobj) => hostobj.has_property(ee, property_name),
        }
    }

    fn delete(&mut self,
              ee: &mut ExecutionEngine,
              property_name: InternedString,
              should_throw: bool)
              -> EvalResult<bool> {
        match *self {
            Object::Standard(ref mut stdobj) => stdobj.delete(ee, property_name, should_throw),
            Object::HostObject(ref mut hostobj) => hostobj.delete(ee, property_name, should_throw),
        }
    }

    fn default_value(&mut self, ee: &mut ExecutionEngine, hint: &RootedValue) -> RootedValue {
        match *self {
            Object::Standard(ref mut stdobj) => stdobj.default_value(ee, hint),
            Object::HostObject(ref mut hostobj) => hostobj.default_value(ee, hint),
        }
    }

    fn define_own_property(&mut self,
                           ee: &mut ExecutionEngine,
                           property_name: InternedString,
                           property: Property,
                           should_throw: bool)
                           -> bool {
        match *self {
            Object::Standard(ref mut stdobj) => {
                stdobj.define_own_property(ee, property_name, property, should_throw)
            }
            Object::HostObject(ref mut hostobj) => {
                hostobj.define_own_property(ee, property_name, property, should_throw)
            }
        }
    }
}

#[derive(Copy, Clone)]
pub enum Property {
    Named {
        value: Option<Value>,
        writable: Option<bool>,
        enumerable: Option<bool>,
        configurable: Option<bool>,
    },
    Accessor {
        get: Option<Value>,
        set: Option<Value>,
        enumerable: Option<bool>,
        configurable: Option<bool>,
    },
}

impl Trace for Property {
    fn trace(&self) -> IntoIter<HeapObject> {
        let mut pointers = vec![];
        match *self {
            Property::Named { value: Some(value), .. } => {
                if let Some(ptr) = value.to_heap_object() {
                    pointers.push(ptr);
                }
            }
            Property::Accessor { get: maybe_get, set: maybe_set, .. } => {
                if let Some(get) = maybe_get {
                    if let Some(ptr) = get.to_heap_object() {
                        pointers.push(ptr);
                    }
                }

                if let Some(set) = maybe_set {
                    if let Some(ptr) = set.to_heap_object() {
                        pointers.push(ptr);
                    }
                }
            }
            _ => {}
        }

        pointers.into_iter()
    }
}

impl Property {
    /// IsAccessorDescriptor, section 8.10.1
    fn is_accessor_descriptor(&self) -> bool {
        if let Property::Accessor { get, set, .. } = *self {
            // according to the spec, this should still return false if
            // both get and set are not present.
            if !get.is_none() || !set.is_none() {
                return true;
            }
        }

        return false;
    }

    /// IsDataDescriptor, section 8.10.2
    fn is_data_descriptor(&self) -> bool {
        // SPEC_NOTE: the spec says that [[Writable]] can be absent, but it's defined
        // to just be a boolean. Does that mean it's simply false?
        if let Property::Named { value, writable, .. } = *self {
            if !value.is_none() || !writable.is_none() {
                return true;
            }
        }

        return false;
    }

    /// IsGenericDescriptor, section 8.10.3
    fn is_generic_descriptor(&self) -> bool {
        !self.is_accessor_descriptor() && !self.is_data_descriptor()
    }

    /// FromPropertyDescriptor, section 8.10.4
    fn from_property_descriptor(&self) -> Object {
        unimplemented!()
    }

    /// ToPropertyDescriptor, section 8.10.5
    fn to_property_descriptor(object: Object) {
        unimplemented!()
    }

    /// Retrieves a property value from a named property, panicking
    /// if the property is either not a named property or lacks a [[Value]]
    /// field.
    fn unwrap_value(&self) -> Value {
        if let Property::Named { value, .. } = *self {
            if let Some(actual_value) = value {
                return actual_value;
            }
        }

        panic!("unwrap_value called on invalid property");
    }

    fn unwrap_get(&self) -> Value {
        if let Property::Accessor { get, .. } = *self {
            if let Some(actual_get) = get {
                return actual_get;
            }
        }

        panic!("unwrap_get called on invalid property");
    }

    fn unwrap_set(&self) -> Value {
        if let Property::Accessor { set, .. } = *self {
            if let Some(actual_set) = set {
                return actual_set;
            }
        }

        panic!("unwrap_set called on invalid property");
    }

    fn unwrap_writable(&self) -> bool {
        if let Property::Named { writable, .. } = *self {
            if let Some(actual_writable) = writable {
                return actual_writable;
            }
        }

        panic!("unwrap_writable called on invalid property");
    }

    fn unwrap_configurable(&self) -> bool {
        let configurable = match *self {
            Property::Named { configurable, .. } => configurable,
            Property::Accessor { configurable, .. } => configurable,
        };

        if let Some(actual_configurable) = configurable {
            return actual_configurable;
        }

        panic!("unwrap_configurable called on invalid property");
    }
}

#[derive(Default)]
pub struct StandardObject {
    prototype: Value,
    backing_storage: HashMap<InternedString, Property>,
}

impl Trace for StandardObject {
    fn trace(&self) -> IntoIter<HeapObject> {
        let mut pointers: Vec<_> = self.prototype.trace().collect();
        for value in self.backing_storage.values() {
            pointers.extend(value.trace());
        }

        pointers.into_iter()
    }
}

impl HostObject for StandardObject {
    /// [[GetOwnProperty]], Section 8.12.1
    fn get_own_property(&mut self,
                        _: &mut ExecutionEngine,
                        name: InternedString)
                        -> Option<Property> {
        self.backing_storage.get(&name).cloned()
    }

    /// [[GetProperty]], Section 8.12.2
    fn get_property(&mut self, ee: &mut ExecutionEngine, name: InternedString) -> Option<Property> {
        let prop = self.get_own_property(ee, name);
        if prop.is_some() {
            return prop;
        }

        if self.prototype.is_null() {
            return prop;
        }

        // TODO careful - can there be cycles in prototype chains?
        let proto = self.prototype.unwrap_object();
        return proto.borrow_mut().get_own_property(ee, name);
    }

    /// [[Class]], Section 8.6.2, Table 8
    fn class(&mut self, _: &mut ExecutionEngine) -> &'static str {
        return "object";
    }

    /// [[Extensible]], Section 8.6.2, Table 8
    fn is_extensible(&mut self, _: &mut ExecutionEngine) -> bool {
        false
    }

    /// [[Get]], Section 8.12.3
    fn get(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> EvalValue {
        if let Some(desc) = self.get_property(ee, property_name) {
            if desc.is_data_descriptor() {
                let value = desc.unwrap_value();
                return Ok(ee.heap_mut().root_value(value));
            }

            debug_assert!(desc.is_accessor_descriptor());
            let get_func = desc.unwrap_get();
            if get_func.is_undefined() {
                return Ok(ee.heap_mut().root_value(Value::undefined()));
            }

            // TODO get_func.call(ee, get_func)
            unimplemented!()
        }

        return Ok(ee.heap_mut().root_value(Value::undefined()));
    }

    /// [[CanPut]], Section 8.12.4
    fn can_put(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> bool {
        if let Some(own_prop) = self.get_own_property(ee, property_name) {
            if own_prop.is_accessor_descriptor() {
                let set = own_prop.unwrap_set();
                return !set.is_undefined();
            }

            debug_assert!(own_prop.is_data_descriptor());
            return own_prop.unwrap_writable();
        }

        if self.prototype.is_null() {
            return self.is_extensible(ee);
        }

        if let Some(inherited_prop) = self.get_property(ee, property_name) {
            if inherited_prop.is_accessor_descriptor() {
                let set = inherited_prop.unwrap_set();
                return !set.is_undefined();
            }

            debug_assert!(inherited_prop.is_data_descriptor());
            if !self.is_extensible(ee) {
                return false;
            }

            return inherited_prop.unwrap_writable();
        }

        return self.is_extensible(ee);
    }

    fn put(&mut self,
           ee: &mut ExecutionEngine,
           property_name: InternedString,
           value: &RootedValue,
           should_throw: bool)
           -> EvalResult<()> {
        if !self.can_put(ee, property_name) {
            if should_throw {
                return ee.throw_type_error("can't assign to this property");
            }

            return Ok(());
        }

        let own_desc = self.get_own_property(ee, property_name)
                           .expect("get_own_property returned None despite can_put being true");
        if own_desc.is_data_descriptor() {
            let value_desc = Property::Named {
                value: Some(value.clone().into_inner()),
                writable: None,
                enumerable: None,
                configurable: None,
            };

            self.define_own_property(ee, property_name, value_desc, should_throw);
            return Ok(());
        }

        let desc = self.get_property(ee, property_name)
                       .expect("get_property returned None despite can_put being true");
        if desc.is_accessor_descriptor() {
            let set = desc.unwrap_set();
            debug_assert!(set.is_undefined());
            // TODO set.call(ee, value)
            unimplemented!()
        }

        let new_prop = Property::Named {
            value: Some(value.clone().into_inner()),
            writable: Some(true),
            enumerable: Some(true),
            configurable: Some(true),
        };

        self.define_own_property(ee, property_name, new_prop, should_throw);
        return Ok(());
    }

    fn has_property(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> bool {
        self.get_property(ee, property_name).is_some()
    }

    fn delete(&mut self,
              ee: &mut ExecutionEngine,
              property_name: InternedString,
              should_throw: bool)
              -> EvalResult<bool> {
        if let Some(desc) = self.get_property(ee, property_name) {
            if desc.unwrap_configurable() {
                self.backing_storage.remove(&property_name);
                return Ok(true);
            }

            if should_throw {
                ee.throw_type_error("cannot delete property")
            } else {
                Ok(false)
            }
        } else {
            return Ok(false);
        }
    }

    fn default_value(&mut self, ee: &mut ExecutionEngine, hint: &RootedValue) -> RootedValue {
        unimplemented!()
    }

    fn define_own_property(&mut self,
                           ee: &mut ExecutionEngine,
                           name: InternedString,
                           prop: Property,
                           should_throw: bool)
                           -> bool {
        unimplemented!()
    }

    fn get_prototype(&mut self, ee: &mut ExecutionEngine) -> RootedValue {
        ee.heap_mut().root_value(self.prototype)
    }

    fn set_prototype(&mut self, _: &mut ExecutionEngine, value: &RootedValue) {
        self.prototype = value.clone().into_inner();
    }
}

pub trait HostObject : Trace {
    fn get_prototype(&mut self, ee: &mut ExecutionEngine) -> RootedValue;
    fn set_prototype(&mut self, ee: &mut ExecutionEngine, value: &RootedValue);
    fn class(&mut self, ee: &mut ExecutionEngine) -> &'static str;
    fn is_extensible(&mut self, ee: &mut ExecutionEngine) -> bool;
    fn get(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> EvalValue;
    fn get_own_property(&mut self,
                        ee: &mut ExecutionEngine,
                        property_name: InternedString)
                        -> Option<Property>;
    fn get_property(&mut self, ee: &mut ExecutionEngine, name: InternedString) -> Option<Property>;
    fn put(&mut self,
           ee: &mut ExecutionEngine,
           property_name: InternedString,
           value: &RootedValue,
           should_throw: bool)
           -> EvalResult<()>;
    fn can_put(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> bool;
    fn has_property(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> bool;
    fn delete(&mut self,
              ee: &mut ExecutionEngine,
              property_name: InternedString,
              should_throw: bool)
              -> EvalResult<bool>;
    fn default_value(&mut self, ee: &mut ExecutionEngine, hint: &RootedValue) -> RootedValue;
    fn define_own_property(&mut self,
                           ee: &mut ExecutionEngine,
                           property_name: InternedString,
                           property: Property,
                           should_throw: bool)
                           -> bool;
}
