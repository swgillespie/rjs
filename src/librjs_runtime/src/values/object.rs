//! Objects are the cornerstone of ECMAScript and are the key to its versatility. Objects
//! themselves can be made of many things:
//! * `StandardObjects`, or regular objects as created by object literals,
//! * `FunctionObjects`, or function objects that are created from function literals or
//!    function declarations,
//! * Others which have yet to be written.

#![allow(dead_code)]

use super::super::heap::{Trace, HeapObject};
use super::super::compiler::InternedString;
use super::super::exec::engine::ExecutionEngine;
use super::{Value, RootedValue, EvalValue, EvalResult};

use values::property::Property;

use std::vec::IntoIter;
use std::default::Default;
use std::collections::HashMap;

macro_rules! reject {
    ($ee:ident, $should_throw:ident) => {
        if $should_throw {
            return $ee.throw_type_error("invalid call to define own property")
        } else {
            return Ok(false)
        }
    }
}

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
                           -> EvalResult<bool> {
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

pub trait PropertyMap {
    fn get_raw_property(&self,
                        ee: &mut ExecutionEngine,
                        name: InternedString)
                        -> Option<&Property>;
    fn get_raw_property_mut(&mut self,
                            ee: &mut ExecutionEngine,
                            name: InternedString)
                            -> Option<&mut Property>;
    fn remove_property(&mut self, ee: &mut ExecutionEngine, name: InternedString);
    fn add_property(&mut self, ee: &mut ExecutionEngine, name: InternedString, prop: Property);

    fn get_prototype_property(&self, ee: &mut ExecutionEngine) -> &Property {
        let proto = ee.interner_mut().intern("prototype");
        self.get_raw_property(ee, proto)
            .expect("all objects should have a prototype")
    }

    fn get_prototype_property_mut(&mut self, ee: &mut ExecutionEngine) -> &mut Property {
        let proto = ee.interner_mut().intern("prototype");
        self.get_raw_property_mut(ee, proto)
            .expect("all objects should have a prototype")
    }
}

pub fn object_get_own_property<T: PropertyMap>(obj: &mut T,
                                               ee: &mut ExecutionEngine,
                                               name: InternedString)
                                               -> Option<Property> {
    obj.get_raw_property(ee, name).cloned()
}

pub fn object_get_property<T: PropertyMap + HostObject>(obj: &mut T,
                                                        ee: &mut ExecutionEngine,
                                                        name: InternedString)
                                                        -> Option<Property> {
    let prop = object_get_own_property(obj, ee, name);
    if prop.is_some() {
        return prop;
    }

    let proto = obj.get_prototype_property(ee).unwrap_value();
    if proto.is_null() {
        return prop;
    }

    // TODO careful - can there be cycles in prototype chains?
    let proto_obj = proto.unwrap_object();
    return proto_obj.borrow_mut().get_own_property(ee, name);
}

pub fn object_class() -> &'static str {
    "object"
}

pub fn object_is_extensible() -> bool {
    false
}

pub fn object_get<T: PropertyMap + HostObject>(obj: &mut T,
                                               ee: &mut ExecutionEngine,
                                               name: InternedString)
                                               -> EvalValue {
    if let Some(desc) = obj.get_property(ee, name) {
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

pub fn object_can_put<T: PropertyMap + HostObject>(obj: &mut T,
                                                   ee: &mut ExecutionEngine,
                                                   property_name: InternedString)
                                                   -> bool {
    if let Some(own_prop) = obj.get_own_property(ee, property_name) {
        if own_prop.is_accessor_descriptor() {
            let set = own_prop.unwrap_set();
            return !set.is_undefined();
        }

        debug_assert!(own_prop.is_data_descriptor());
        return own_prop.unwrap_writable();
    }

    let proto = obj.get_prototype_property(ee).unwrap_value();
    if proto.is_null() {
        return obj.is_extensible(ee);
    }

    if let Some(inherited_prop) = obj.get_property(ee, property_name) {
        if inherited_prop.is_accessor_descriptor() {
            let set = inherited_prop.unwrap_set();
            return !set.is_undefined();
        }

        debug_assert!(inherited_prop.is_data_descriptor());
        if !obj.is_extensible(ee) {
            return false;
        }

        return inherited_prop.unwrap_writable();
    }

    return obj.is_extensible(ee);
}

pub fn object_put<T: PropertyMap + HostObject>(obj: &mut T,
                                               ee: &mut ExecutionEngine,
                                               property_name: InternedString,
                                               value: &RootedValue,
                                               should_throw: bool)
                                               -> EvalResult<()> {
    if !obj.can_put(ee, property_name) {
        if should_throw {
            return ee.throw_type_error("can't assign to this property");
        }

        return Ok(());
    }

    let own_desc = obj.get_own_property(ee, property_name)
                      .expect("get_own_property returned None despite can_put being true");
    if own_desc.is_data_descriptor() {
        let value_desc = Property::Named {
            value: Some(value.clone().into_inner()),
            writable: None,
            enumerable: None,
            configurable: None,
        };

        return obj.define_own_property(ee, property_name, value_desc, should_throw)
                  .map(|_| ());
    }

    let desc = obj.get_property(ee, property_name)
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

    obj.define_own_property(ee, property_name, new_prop, should_throw)
       .ok()
       .expect("define_own_property should not fail from put");
    return Ok(());
}

pub fn object_has_property<T: HostObject>(obj: &mut T,
                                          ee: &mut ExecutionEngine,
                                          property_name: InternedString)
                                          -> bool {
    obj.get_property(ee, property_name).is_some()
}

pub fn object_delete<T: PropertyMap + HostObject>(obj: &mut T,
                                                  ee: &mut ExecutionEngine,
                                                  property_name: InternedString,
                                                  should_throw: bool)
                                                  -> EvalResult<bool> {
    if let Some(desc) = obj.get_property(ee, property_name) {
        if desc.unwrap_configurable() {
            obj.remove_property(ee, property_name);
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

pub fn object_default_value<T>(_: &mut T, _: &mut ExecutionEngine, _: &RootedValue) -> RootedValue {
    unimplemented!()
}

pub fn object_define_own_property<T: PropertyMap + HostObject>(obj: &mut T,
                                                               ee: &mut ExecutionEngine,
                                                               name: InternedString,
                                                               prop: Property,
                                                               should_throw: bool)
                                                               -> EvalResult<bool> {
    if let Some(current) = obj.get_own_property(ee, name) {
        // if all properties are abstent on the prop, return true.
        match prop {
            Property::Named {
                    value: None,
                    writable: None,
                    enumerable: None,
                    configurable: None
                } |
            Property::Accessor {
                    get: None,
                    set: None,
                    enumerable: None,
                    configurable: None } => return Ok(true),
            _ => {}
        }

        // if all properties in the current property are identical to
        // the provided property, return true.
        // TODO this is not a cheap check - need to see if we can move this
        // to a colder code path while still maintaining correctness.
        if property_is_same(current, prop) {
            return Ok(true);
        }

        if !current.unwrap_configurable() {
            if prop.unwrap_configurable() {
                reject!(ee, should_throw)
            }

            if let Some(prop_enumerable) = prop.try_get_enumerable() {
                if current.unwrap_enumerable() != prop_enumerable {
                    reject!(ee, should_throw)
                }
            }
        }

        if current.is_data_descriptor() != prop.is_data_descriptor() {
            if !current.unwrap_configurable() {
                reject!(ee, should_throw);
            }

            if current.is_data_descriptor() {
                let new_prop = Property::new_accesor_descriptor(current.unwrap_enumerable(),
                                                                current.unwrap_configurable());
                obj.add_property(ee, name, new_prop);
                return Ok(true);
            } else {
                debug_assert!(current.is_accessor_descriptor());
                // SPEC_NOTE: is this new property writable? default to yes.
                let new_prop = Property::new_data_descriptor(true,
                                                             current.unwrap_enumerable(),
                                                             current.unwrap_configurable());
                obj.add_property(ee, name, new_prop);
                return Ok(true);
            }
        }

        if current.is_data_descriptor() && prop.is_data_descriptor() {
            if !current.unwrap_configurable() {
                if let Some(prop_writable) = prop.try_get_writable() {
                    if !current.unwrap_writable() && prop_writable {
                        reject!(ee, should_throw);
                    }

                    if !current.unwrap_writable() {
                        if let Some(prop_value) = prop.try_get_value() {
                            if !current.unwrap_value().same_value(prop_value) {
                                reject!(ee, should_throw);
                            }
                        }
                    }
                }
            }
        } else {
            debug_assert!(current.is_accessor_descriptor());
            debug_assert!(prop.is_accessor_descriptor());
            if !current.unwrap_configurable() {
                if let Some(prop_set) = prop.try_get_set() {
                    if !current.unwrap_value().same_value(prop_set) {
                        reject!(ee, should_throw);
                    }
                }

                if let Some(prop_get) = prop.try_get_get() {
                    if !current.unwrap_value().same_value(prop_get) {
                        reject!(ee, should_throw);
                    }
                }
            }
        }
    } else {
        if !obj.is_extensible(ee) {
            reject!(ee, should_throw);
        }
    }

    // TODO populate default values?
    obj.add_property(ee, name, prop);
    return Ok(true);
}

#[derive(Default)]
pub struct StandardObject {
    backing_storage: HashMap<InternedString, Property>,
}

impl Trace for StandardObject {
    fn trace(&self) -> IntoIter<HeapObject> {
        let mut pointers = vec![];
        for value in self.backing_storage.values() {
            pointers.extend(value.trace());
        }

        pointers.into_iter()
    }
}

impl PropertyMap for StandardObject {
    fn get_raw_property(&self, _: &mut ExecutionEngine, name: InternedString) -> Option<&Property> {
        self.backing_storage.get(&name)
    }

    fn get_raw_property_mut(&mut self,
                            _: &mut ExecutionEngine,
                            name: InternedString)
                            -> Option<&mut Property> {
        self.backing_storage.get_mut(&name)
    }

    fn remove_property(&mut self, _: &mut ExecutionEngine, name: InternedString) {
        self.backing_storage.remove(&name);
    }

    fn add_property(&mut self, _: &mut ExecutionEngine, name: InternedString, prop: Property) {
        self.backing_storage.insert(name, prop);
    }
}

impl HostObject for StandardObject {
    /// [[GetOwnProperty]], Section 8.12.1
    fn get_own_property(&mut self,
                        ee: &mut ExecutionEngine,
                        name: InternedString)
                        -> Option<Property> {
        object_get_own_property(self, ee, name)
    }

    /// [[GetProperty]], Section 8.12.2
    fn get_property(&mut self, ee: &mut ExecutionEngine, name: InternedString) -> Option<Property> {
        object_get_property(self, ee, name)
    }

    /// [[Class]], Section 8.6.2, Table 8
    fn class(&mut self, _: &mut ExecutionEngine) -> &'static str {
        object_class()
    }

    /// [[Extensible]], Section 8.6.2, Table 8
    fn is_extensible(&mut self, _: &mut ExecutionEngine) -> bool {
        object_is_extensible()
    }

    /// [[Get]], Section 8.12.3
    fn get(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> EvalValue {
        object_get(self, ee, property_name)
    }

    /// [[CanPut]], Section 8.12.4
    fn can_put(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> bool {
        object_can_put(self, ee, property_name)
    }

    fn put(&mut self,
           ee: &mut ExecutionEngine,
           property_name: InternedString,
           value: &RootedValue,
           should_throw: bool)
           -> EvalResult<()> {
        object_put(self, ee, property_name, value, should_throw)
    }

    fn has_property(&mut self, ee: &mut ExecutionEngine, property_name: InternedString) -> bool {
        object_has_property(self, ee, property_name)
    }

    fn delete(&mut self,
              ee: &mut ExecutionEngine,
              property_name: InternedString,
              should_throw: bool)
              -> EvalResult<bool> {
        object_delete(self, ee, property_name, should_throw)
    }

    fn default_value(&mut self, ee: &mut ExecutionEngine, hint: &RootedValue) -> RootedValue {
        object_default_value(self, ee, hint)
    }

    /// [[DefineOwnProperty]], section 8.12.9
    fn define_own_property(&mut self,
                           ee: &mut ExecutionEngine,
                           name: InternedString,
                           prop: Property,
                           should_throw: bool)
                           -> EvalResult<bool> {
        object_define_own_property(self, ee, name, prop, should_throw)
    }
}

/// This is some seriously bad Rust that does the property sameness check as mandated by
/// the spec. It's not clear whether or not this is necessary and hopefully I can remove this
/// at some point.
fn property_is_same(current: Property, prop: Property) -> bool {
    match (current, prop) {
        (Property::Named { value: current_value, writable: current_writable, enumerable: current_enumerable, configurable: current_configurable },
         Property::Named { value: prop_value, writable: prop_writable, enumerable: prop_enumerable, configurable: prop_configurable }) => {
             if let Some(actual_current_value) = current_value {
                 if let Some(actual_prop_value) = prop_value {
                     if actual_current_value.same_value(actual_prop_value) && current_writable == prop_writable && current_enumerable == prop_enumerable && current_configurable == prop_configurable {
                         return true;
                     }
                 }
             }
         }
        (Property::Accessor { get: current_get, set: current_set, enumerable: current_enumerable, configurable: current_configurable },
          Property::Accessor { get: prop_get, set: prop_set, enumerable: prop_enumerable, configurable: prop_configurable }) => {
              match (current_get, current_set, prop_get, prop_set) {
                  (Some(c_get), Some(c_set), Some(p_get), Some(p_set)) => {
                      if c_get.same_value(p_get) && c_set.same_value(p_set) && current_enumerable == prop_enumerable && current_configurable == prop_configurable {
                          return true;
                      }
                  },
                  _ => {}
              }
         }
        _ => {}
    }

    return false;
}

pub trait HostObject : Trace {
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
                           -> EvalResult<bool>;

    fn call(&mut self, ee: &mut ExecutionEngine, _: Vec<RootedValue>, _: RootedValue) -> EvalValue {
        ee.throw_type_error("value is not callable")
    }

    fn construct(&mut self, ee: &mut ExecutionEngine, _: Vec<RootedValue>) -> EvalValue {
        ee.throw_type_error("value is not constructable")
    }

    fn has_instance(&mut self, ee: &mut ExecutionEngine, _: &RootedValue) -> EvalResult<bool> {
        ee.throw_type_error("value does not have has_instance")
    }

    fn primitive_value(&mut self, ee: &mut ExecutionEngine) -> EvalValue {
        ee.throw_type_error("value does not have primitive value")
    }
}
