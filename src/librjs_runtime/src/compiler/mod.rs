//! The compiler targeting the VM's bytecode.

pub mod bytecode;
pub mod hir;
mod hir_builder;
mod string_interer;
mod emitter;
mod bytecode_builder;

pub use self::emitter::{CompiledFunction, CompiledProgram};
pub use self::string_interer::{InternedString, StringInterner};

use librjs_syntax::ast::{Program, SpannedStatement};

pub fn lower_program_to_hir(interner: &mut string_interer::StringInterner,
                            program: &Program)
                            -> hir::Program {
    let mut builder = hir_builder::HirBuilder::new(interner);
    builder.lower_program(program)
}

pub fn lower_statement_to_hir(interner: &mut string_interer::StringInterner,
                              stmt: &SpannedStatement)
                              -> hir::Statement {
    let mut builder = hir_builder::HirBuilder::new(interner);
    builder.lower_statement(stmt)
}

pub fn lower_hir_to_bytecode(interner: string_interer::StringInterner,
                             hir: &hir::Program)
                             -> emitter::CompiledProgram {
    let builder = bytecode_builder::BytecodeBuilder::new(interner);
    builder.lower_program(hir)
}
