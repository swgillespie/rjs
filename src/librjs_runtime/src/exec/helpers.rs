use exec::engine::ExecutionEngine;
use values::{RootedValue, Value};
use heap::RootedObjectPtr;

use std::f64;

pub fn to_number(ee: &mut ExecutionEngine, value: &RootedValue) -> f64 {
    match **value {
        Value::Undefined => f64::NAN,
        Value::Null => 0f64,
        Value::Boolean(b) => {
            if *b.borrow() {
                1f64
            } else {
                0f64
            }
        }
        Value::Number(num) => *num.borrow(),
        Value::String(_) => 42f64, // TODO
        Value::Object(_) => {
            let prim = to_primitive(ee, value);
            to_number(ee, &prim)
        }
    }
}

pub fn to_boolean(_: &mut ExecutionEngine, value: &RootedValue) -> bool {
    match **value {
        Value::Undefined | Value::Null => false,
        Value::Boolean(b) => *b.borrow(),
        Value::Number(num) => {
            // TODO can't pattern match on negative zero?
            let value = *num.borrow();
            value != 0f64 && value != -0f64 && !value.is_nan()
        }
        Value::String(s) => s.borrow().len() != 0,
        Value::Object(_) => true,
    }
}

pub fn to_string(ee: &mut ExecutionEngine, value: &RootedValue) -> String {
    match **value {
        Value::Undefined => "undefined".to_string(),
        Value::Null => "null".to_string(),
        Value::Boolean(b) => {
            if *b.borrow() {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        Value::Number(n) => n.borrow().to_string(), // TODO
        Value::String(s) => s.borrow().clone(),
        Value::Object(_) => {
            let prim = to_primitive(ee, value);
            to_string(ee, &prim)
        }
    }
}

pub fn to_int32(ee: &mut ExecutionEngine, value: &RootedValue) -> i32 {
    let num = to_number(ee, value);
    if num.is_nan() || num == 0f64 || num == -0f64 || num.is_infinite() {
        return 0;
    }

    let pos_int = (num.signum() * num.abs().floor()) as i64;
    let int32bit = (pos_int & (2 ^ 32 - 1)) as i32;
    if int32bit > 2 ^ 31 {
        int32bit - 2 ^ 32
    } else {
        int32bit
    }
}

pub fn to_uint32(ee: &mut ExecutionEngine, value: &RootedValue) -> u32 {
    let num = to_number(ee, value);
    if num.is_nan() || num == 0f64 || num == -0f64 || num.is_infinite() {
        return 0;
    }

    let pos_int = (num.signum() * num.abs().floor()) as i64;
    (pos_int & (2 ^ 32 - 1)) as u32
}

pub fn to_object(_: &mut ExecutionEngine, _: &RootedValue) -> RootedObjectPtr {
    unimplemented!()
}

pub fn type_of(_: &mut ExecutionEngine, _: &RootedValue) -> String {
    unimplemented!()
}

pub fn to_primitive(_: &mut ExecutionEngine, _: &RootedValue) -> RootedValue {
    unimplemented!()
}

pub fn equals(_: &mut ExecutionEngine, _: &RootedValue, _: &RootedValue) -> bool {
    unimplemented!()
}

pub fn strict_equals(_: &mut ExecutionEngine, _: &RootedValue, _: &RootedValue) -> bool {
    unimplemented!()
}

pub fn greater_than(_: &mut ExecutionEngine, _: &RootedValue, _: &RootedValue) -> bool {
    unimplemented!()
}
