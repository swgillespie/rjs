//! A frame consists of an activation record and a function object being executed.
//! The list of frames maintained by the ExecutionEngine forms the "stack".

use heap::RootedActivationPtr;
use compiler::CompiledFunction;

use std::rc::Rc;

pub struct Frame {
    pub activation: RootedActivationPtr,
    pub code_object: Rc<CompiledFunction>,
}

impl Frame {
    pub fn new(activation: RootedActivationPtr, code_object: CompiledFunction) -> Frame {
        Frame {
            activation: activation,
            code_object: Rc::new(code_object),
        }
    }
}
