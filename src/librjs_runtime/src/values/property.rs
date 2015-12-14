use values::Value;
use values::object::Object;
use heap::{HeapObject, Trace, ToHeapObject};

use std::vec::IntoIter;

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
    pub fn new_accesor_descriptor(enumerable: bool, configurable: bool) -> Property {
        Property::Accessor {
            get: Some(Value::undefined()),
            set: Some(Value::undefined()),
            enumerable: Some(enumerable),
            configurable: Some(configurable),
        }
    }

    pub fn new_data_descriptor(enumerable: bool, configurable: bool) -> Property {
        Property::Named {
            value: Some(Value::undefined()),
            writable: Some(false),
            enumerable: Some(enumerable),
            configurable: Some(configurable),
        }
    }

    /// IsAccessorDescriptor, section 8.10.1
    pub fn is_accessor_descriptor(&self) -> bool {
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
    pub fn is_data_descriptor(&self) -> bool {
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
    pub fn is_generic_descriptor(&self) -> bool {
        !self.is_accessor_descriptor() && !self.is_data_descriptor()
    }

    /// FromPropertyDescriptor, section 8.10.4
    pub fn from_property_descriptor(&self) -> Object {
        unimplemented!()
    }

    /// ToPropertyDescriptor, section 8.10.5
    pub fn to_property_descriptor(object: Object) {
        unimplemented!()
    }

    /// Retrieves a property value from a named property, panicking
    /// if the property is either not a named property or lacks a [[Value]]
    /// field.
    pub fn unwrap_value(&self) -> Value {
        if let Property::Named { value, .. } = *self {
            if let Some(actual_value) = value {
                return actual_value;
            }
        }

        panic!("unwrap_value called on invalid property");
    }

    pub fn try_get_value(&self) -> Option<Value> {
        if let Property::Named { value, .. } = *self {
            return value;
        }

        None
    }

    pub fn unwrap_get(&self) -> Value {
        if let Property::Accessor { get, .. } = *self {
            if let Some(actual_get) = get {
                return actual_get;
            }
        }

        panic!("unwrap_get called on invalid property");
    }

    pub fn try_get_get(&self) -> Option<Value> {
        if let Property::Accessor { get, .. } = *self {
            return get;
        }

        None
    }

    pub fn unwrap_set(&self) -> Value {
        if let Property::Accessor { set, .. } = *self {
            if let Some(actual_set) = set {
                return actual_set;
            }
        }

        panic!("unwrap_set called on invalid property");
    }

    pub fn try_get_set(&self) -> Option<Value> {
        if let Property::Accessor { set, .. } = *self {
            return set;
        }

        None
    }

    pub fn unwrap_writable(&self) -> bool {
        if let Property::Named { writable, .. } = *self {
            if let Some(actual_writable) = writable {
                return actual_writable;
            }
        }

        panic!("unwrap_writable called on invalid property");
    }

    pub fn try_get_writable(&self) -> Option<bool> {
        if let Property::Named { writable, .. } = *self {
            return writable;
        }

        return None;
    }

    pub fn unwrap_configurable(&self) -> bool {
        let configurable = match *self {
            Property::Named { configurable, .. } => configurable,
            Property::Accessor { configurable, .. } => configurable,
        };

        if let Some(actual_configurable) = configurable {
            return actual_configurable;
        }

        panic!("unwrap_configurable called on invalid property");
    }

    pub fn unwrap_enumerable(&self) -> bool {
        let enumerable = match *self {
            Property::Named { enumerable, .. } => enumerable,
            Property::Accessor { enumerable, .. } => enumerable,
        };

        if let Some(actual_enumerable) = enumerable {
            return actual_enumerable;
        }

        panic!("unwrap_enumerable called on invalid property");
    }

    pub fn try_get_enumerable(&self) -> Option<bool> {
        match *self {
            Property::Named { enumerable, .. } => enumerable,
            Property::Accessor { enumerable, .. } => enumerable,
        }
    }
}
