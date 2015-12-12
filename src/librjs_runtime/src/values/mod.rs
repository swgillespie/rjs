//! This module contains type definition for ECMAScript values.
//! According to the ECMA-262 specification, there are six "types"
//! of objects: `undefined`, `null`, `Boolean`, `String`, `Number`,
//! and `Object`. Four of these are value types and do not need
//! heap allocation, while `String` and `Object` have to be allocated
//! on the heap.
//!
//! ## Undefined
//! The undefined type is a type with exactly one value: `undefined`.
//! It is the value of any unassigned variable.
//!
//! ## Null
//! The null type if also a type with exactly one value: `null`.
//!
//! ## Boolean
//! The boolean type has exactly two values: `true` and `false`.
//!
//! ## String
//! The string type is defined by the spec to be the set of
//! all finite sequences of 16-bit integers. Unlike Rust strings,
//! ECMAScript strings can be indexed, which returns the n-th
//! 16-bit integer that composes this string.
//!
//! When a string contains textual data, it's assumed by the
//! implementation and the spec that each 16-bit integer is a
//! valid UTF-16 code unit.
//!
//! ## Number
//! The number type is defined by the spec as a 64-bit floating
//! point number. As such, there exist many ways to represent a
//! floating point `NaN`, as well as exactly one `Infinity` and
//! `-Infinity`.
//!
//! ECMAScript acknowledges the existence of both a positive
//! zero and negative zero.
//!
//! Some ECMAScript operations work on 32-bit integers explicitly.
//! In this case, the implementation will convert a Number value
//! into an integer using the internal *ToInt32* and *ToUInt32* conversion
//! functions to convert the numeric value.
//!
//! ## Object
//! You could write books about this type, so I'll keep this brief.
//! An object is a collection of properties. Every property is one
//! of three types:
//! * A **named data property**, which associates a *name* with an
//!   ECMAScript language value and a set of attributes.
//! * A **named accessor property**, which associates a *name* with
//!   one or two *acessor functions* (getter/setter), along with a
//!   set of attributes.
//! * An **internal property**, which isn't exposed to ECMAScript and
//!   is used for specification and implementation purposes.

mod object;
mod activation;

use super::heap::{self, RootedPtr, ToHeapObject, HeapObject, Trace};
use std::vec::IntoIter;
use std::default::Default;

pub use self::object::Object;
pub use self::activation::Activation;

pub type RootedValue = RootedPtr<Value>;

#[derive(Copy, Clone)]
pub enum Value {
    // `undefined`, the sentinel of ECMAScript
    Undefined,
    // `null`, the value of the null object
    Null,
    // numbers
    Number(heap::NumberPtr),
    NumberObject(heap::NumberPtr),
    // booleans
    Boolean(heap::BooleanPtr),
    BooleanObject(heap::BooleanPtr),
    // strings
    String(heap::StringPtr),
    StringObject(heap::StringPtr),
    // objects
    Object(heap::ObjectPtr)
}

impl ToHeapObject for Value {
    fn to_heap_object(&self) -> Option<HeapObject> {
        match *self {
            Value::Null | Value::Undefined => None,
            Value::Number(ptr) | Value::NumberObject(ptr) => ptr.to_heap_object(),
            Value::Boolean(ptr) | Value::BooleanObject(ptr) => ptr.to_heap_object(),
            Value::String(ptr) | Value::StringObject(ptr) => ptr.to_heap_object(),
            Value::Object(ptr) => ptr.to_heap_object()
        }
    }
}

impl Trace for Value {
    fn trace(&self) -> IntoIter<HeapObject> {
        if let Some(heap_obj) = self.to_heap_object() {
            heap_obj.trace()
        } else {
            vec![].into_iter()
        }
    }
}

impl Default for Value {
    fn default() -> Value {
        Value::Undefined
    }
}

impl Value {
    pub fn undefined() -> Value {
        Value::Undefined
    }

    pub fn null() -> Value {
        Value::Null
    }

    pub fn number(ptr: heap::NumberPtr) -> Value {
        Value::Number(ptr)
    }

    pub fn number_object(ptr: heap::NumberPtr) -> Value {
        Value::NumberObject(ptr)
    }

    pub fn boolean(ptr: heap::BooleanPtr) -> Value {
        Value::Boolean(ptr)
    }

    pub fn boolean_object(ptr: heap::BooleanPtr) -> Value {
        Value::BooleanObject(ptr)
    }

    pub fn string(ptr: heap::StringPtr) -> Value {
        Value::String(ptr)
    }

    pub fn string_object(ptr: heap::StringPtr) -> Value {
        Value::StringObject(ptr)
    }

    pub fn object(ptr: heap::ObjectPtr) -> Value {
        Value::Object(ptr)
    }

    pub fn is_undefined(&self) -> bool {
        if let Value::Undefined = *self {
            true
        } else {
            false
        }
    }

    pub fn is_null(&self) -> bool {
        if let Value::Null = *self {
            true
        } else {
            false
        }
    }

    pub fn unwrap_object(&self) -> heap::ObjectPtr {
        if let Value::Object(ptr) = *self {
            return ptr;
        }

        panic!("unwrap_object called on non-object value");
    }
}

pub struct Exception;
pub type EvalResult<T> = Result<T, Exception>;
pub type EvalValue = Result<RootedValue, Exception>;
