//! The execution engine is responsible for actually executing ECMAScript programs.
use heap::Heap;
use values::{RootedValue, EvalValue};

pub struct ExecutionEngine {
    heap: Heap
}

impl ExecutionEngine {
    pub fn new() {
        unimplemented!()
    }

    pub fn call(&mut self, function: &RootedValue, arguments: Vec<&RootedValue>) -> EvalValue {
        unimplemented!()
    }

    pub fn eval(&mut self, program: &RootedValue) -> EvalValue {
        unimplemented!()
    }

    pub fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }

    pub fn heap(&self) -> &Heap {
        &self.heap
    }
}
