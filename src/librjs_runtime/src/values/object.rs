//! Objects are the cornerstone of ECMAScript and are the key to its versatility. Objects
//! themselves can be made of many things:
//! * `StandardObjects`, or regular objects as created by object literals,
//! * `FunctionObjects`, or function objects that are created from function literals or
//!    function declarations,
//! * Others which have yet to be written.

use super::super::heap::{Trace, HeapObject};
use super::super::exec::ExecutionEngine;
use super::{RootedValue, Property, EvalValue};

use std::vec::IntoIter;
use std::default::Default;

#[derive(PartialEq)]
pub enum Object {
    Standard
}

impl Default for Object {
    fn default() -> Object {
        // TODO
        Object::Standard
    }
}

impl Trace for Object {
    fn trace(&self) -> IntoIter<HeapObject> {
        // TODO
        vec![].into_iter()
    }
}

pub trait HostObject : Trace {
    fn get_prototype(&self, ee: &mut ExecutionEngine) -> RootedValue;
    fn set_prototype(&mut self, value: &RootedValue);
    fn class(&self) -> &'static str;
    fn is_extensible(&self) -> bool;
    fn get(&self, ee: &mut ExecutionEngine, property_name: &str) -> EvalValue;
    fn get_own_property(&self, property_name: &str) -> Option<Property>;
    fn get_property(&self) -> Option<Property>;
    fn put(&mut self, ee: &mut ExecutionEngine, property_name: &str, value: &RootedValue, should_throw: bool);
    fn can_put(&self, property_name: &str) -> bool;
    fn has_property(&self, property_name: &str) -> bool;
    fn delete(&mut self, property_name: &str, should_throw: bool) -> bool;
    fn default_value(&self, hint: &str) -> RootedValue;
    fn define_own_property(&mut self, property_name: &str, property: Property, should_throw: bool) -> bool;
}
