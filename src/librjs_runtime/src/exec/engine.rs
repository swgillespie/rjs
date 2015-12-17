//! The execution engine is responsible for actually executing ECMAScript programs.
#![allow(unused_variables, dead_code)]

use heap::{Heap, RootedActivationPtr};
use values::{EvalResult, RootedValue, EvalValue};
use exec::frame::Frame;
// use values::{RootedValue, EvalValue};
use std::path::Path;
use std::convert::AsRef;
use std::collections::LinkedList;

pub struct ExecutionEngine {
    heap: Heap,
    stack: LinkedList<Frame>,
}

impl ExecutionEngine {
    pub fn new() -> ExecutionEngine {
        ExecutionEngine {
            heap: Heap::new(),
            stack: LinkedList::new(),
        }
    }

    pub fn eval_file<P: AsRef<Path>>(&mut self, file: P) {
        let path = file.as_ref();
        unimplemented!()
    }

    pub fn eval_str<S: AsRef<str>>(&mut self, value: S) {
        let string = value.as_ref();
        unimplemented!()
    }

    pub fn call(&mut self, code_obj: RootedValue, args: Vec<RootedValue>) -> EvalValue {
        unimplemented!()
    }

    pub fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }

    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    pub fn throw_type_error<T>(&self, message: &'static str) -> EvalResult<T> {
        unimplemented!()
    }

    pub fn throw_reference_error<T>(&self, message: &'static str) -> EvalResult<T> {
        unimplemented!()
    }
}
