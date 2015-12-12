//! The execution engine is responsible for actually executing ECMAScript programs.
#![allow(unused_variables, dead_code)]

use heap::Heap;
use values::EvalResult;
// use values::{RootedValue, EvalValue};
use std::path::Path;
use std::convert::AsRef;

pub struct ExecutionEngine {
    heap: Heap,
}

impl ExecutionEngine {
    pub fn new() -> ExecutionEngine {
        ExecutionEngine { heap: Heap::new() }
    }

    pub fn eval_file<P: AsRef<Path>>(&mut self, file: P) {
        let path = file.as_ref();
        unimplemented!()
    }

    pub fn eval_str<S: AsRef<str>>(&mut self, value: S) {
        let string = value.as_ref();
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
}
