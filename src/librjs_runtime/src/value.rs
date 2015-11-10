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
use heap::{StringPtr, ObjectPtr, RootedPtr, Heap, ToHeapObject, HeapObject};

pub type RootedValue = RootedPtr<Value>;
pub enum Value {
    Undefined,
    Null,
    Boolean(bool),
    Number(f64),
    String(StringPtr),
    Object(ObjectPtr)
}

pub trait ToValue {
    fn to_value(&self) -> Value;
}

impl ToHeapObject for Value {
    fn to_heap_object(&self) -> Option<HeapObject> {
        match *self {
            Value::String(ref ptr) => ptr.to_heap_object(),
            Value::Object(ref ptr) => ptr.to_heap_object(),
            _ => None
        }
    }
}

impl Value {
    pub fn new_undefined() -> Value {
        Value::Undefined
    }

    pub fn new_null() -> Value {
        Value::Null
    }

    pub fn new_boolean(value: bool) -> Value {
        Value::Boolean(value)
    }

    pub fn new_number(num: f64) -> Value {
        Value::Number(num)
    }

    pub fn new_string(heap: &mut Heap, value: &str) -> RootedValue {
        // TODO convert value to UTF16 and store the vector
        // in a newly-allocated heap string
        unimplemented!()
    }
}

pub enum Completion {
    Break,
    Continue,
    Normal,
    Return(Value),
    Throw(Value),
}

pub struct Object {
    prototype: Value,
    class: Value,
    extensible: Value
}

/// Section 8.6.1, "Property Attributes"
pub enum Property {
    /// The type of properties that associate a name with
    /// a value.
    NamedData {
        /// The value returned when this property is ready
        value: Value,
        /// Whether or not this property is writable
        writeable: bool,
        /// Whether or not this property can be iterated
        /// by a for-in statement
        enumerable: bool,
        /// If `false`, attempting to `delete` this property,
        /// change the property to be an accessor, or change
        /// any attributes other than `value` are not permitted
        configurable: bool
    },
    NamedAccessor {
        /// A function object invoked when this property is retrieved,
        /// or `undefined`.
        get: Value,
        /// A function object invoked when this property is set,
        /// or `undefined`.
        set: Value,
        /// Whether or not this property can be iterated by a
        /// for-in statement
        enumerable: bool,
        /// If `false`, attempts to delete the property, change
        /// the property to be a data property, or change any
        /// attributes will fail.
        configurable: bool
    }
}

