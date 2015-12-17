//! A frame consists of an activation record and a function object being executed.
//! The list of frames maintained by the ExecutionEngine forms the "stack".

use heap::RootedActivationPtr;
use values::{Value, RootedValue, EvalValue};
use exec::engine::ExecutionEngine;

pub struct Frame {
    activation: RootedActivationPtr,
    code_object: RootedValue,
    interpreter_stack: Vec<Value>,
    instruction_pointer: usize,
}

impl Frame {
    pub fn new(activation: RootedActivationPtr, code_object: RootedValue) -> Frame {
        Frame {
            activation: activation,
            code_object: code_object,
            interpreter_stack: vec![],
            instruction_pointer: 0,
        }
    }

    pub fn execute(&mut self, ee: &mut ExecutionEngine) -> EvalValue {
        unimplemented!()
    }
}
