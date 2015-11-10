#![deny(warnings)]

extern crate librjs_syntax;
extern crate librjs_runtime;

pub mod syntax {
    pub use librjs_syntax::*;
}

pub mod runtime {
    pub use librjs_runtime::*;
}
