//! A frame consists of an activation record and a function object being executed.
//! The list of frames maintained by the ExecutionEngine forms the "stack".

use heap::RootedActivationPtr;

pub struct Frame {
    pub activation: RootedActivationPtr,
    pub name: String,
}

impl Frame {
    pub fn new(activation: RootedActivationPtr, name: String) -> Frame {
        Frame {
            activation: activation,
            name: name,
        }
    }
}
