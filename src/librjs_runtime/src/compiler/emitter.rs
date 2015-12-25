//! This module provides a bytecode emitter for use in the lowering of the HIR
//! into bytecode. The output of the bytecode emitter is a "program", which consists of several
//! things:
//!
//! 1. A table of interned strings, indexable by usize, which is used to refer
//!    to strings in bytecode,
//! 2. A table of functions, indexable by usize, which is used
//!    to refer to functions,
//! 3. A "global function", which is the code that will be run when that program is
//!    evaluated directly.
//!
//! For every function being lowered, the bytecode emitter emits a "function object", which
//! also consists of several things:
//!
//! 1. The name of the function, if it exists,
//! 2. The arity of the function,
//! 3. The list of bytecode operations representing this function.
//!
//!
use super::string_interer::{StringInterner, InternedString};
use super::bytecode::Opcode;

pub type Offset = isize;
pub type Label = usize;

#[derive(Default)]
pub struct CompiledProgram {
    strings: StringInterner,
    functions: Box<[CompiledFunction]>,
    global_functions: Vec<Box<[Opcode]>>,
}

impl CompiledProgram {
    pub fn interner(&self) -> &StringInterner {
        &self.strings
    }

    pub fn interner_mut(&mut self) -> &mut StringInterner {
        &mut self.strings
    }

    pub fn functions(&self) -> &[CompiledFunction] {
        &self.functions
    }

    pub fn function_index(&self, index: usize) -> &CompiledFunction {
        &self.functions[index]
    }

    pub fn current_global_function(&self) -> &[Opcode] {
        self.global_functions.last().expect("should have at least one global function")
    }

    pub fn merge(&mut self, other: CompiledProgram) {
        self.functions = other.functions;
        self.global_functions.extend(other.global_functions);
        self.strings = other.strings;
    }
}

#[derive(Clone, Debug, Default)]
pub struct CompiledFunction {
    name: Option<InternedString>,
    arity: usize,
    code: Box<[Opcode]>,
}

impl CompiledFunction {
    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn name(&self) -> Option<InternedString> {
        self.name
    }

    pub fn code<'a>(&'a self) -> &'a [Opcode] {
        &self.code
    }

    pub fn is_strict(&self) -> bool {
        // TODO
        unimplemented!()
    }
}

pub trait BytecodeEmitter {
    fn emit_opcode(&mut self, opcode: Opcode);
    fn create_label(&mut self) -> Label;
    fn mark_label(&mut self, label: Label);

    fn emit_nop(&mut self) {
        self.emit_opcode(Opcode::Nop);
    }

    fn emit_dup(&mut self) {
        self.emit_opcode(Opcode::Dup);
    }

    fn emit_pop(&mut self) {
        self.emit_opcode(Opcode::Pop);
    }

    fn emit_rotate(&mut self) {
        self.emit_opcode(Opcode::Rotate);
    }

    fn emit_neg(&mut self) {
        self.emit_opcode(Opcode::Neg);
    }

    fn emit_pos(&mut self) {
        self.emit_opcode(Opcode::Pos);
    }

    fn emit_not(&mut self) {
        self.emit_opcode(Opcode::Not);
    }

    fn emit_bitnot(&mut self) {
        self.emit_opcode(Opcode::BitNot);
    }

    fn emit_typeof(&mut self) {
        self.emit_opcode(Opcode::Typeof);
    }

    fn emit_add(&mut self) {
        self.emit_opcode(Opcode::Add);
    }

    fn emit_sub(&mut self) {
        self.emit_opcode(Opcode::Sub);
    }

    fn emit_mul(&mut self) {
        self.emit_opcode(Opcode::Mul);
    }

    fn emit_div(&mut self) {
        self.emit_opcode(Opcode::Div);
    }

    fn emit_mod(&mut self) {
        self.emit_opcode(Opcode::Mod);
    }

    fn emit_bitor(&mut self) {
        self.emit_opcode(Opcode::BitOr);
    }

    fn emit_bitand(&mut self) {
        self.emit_opcode(Opcode::BitAnd);
    }

    fn emit_bitxor(&mut self) {
        self.emit_opcode(Opcode::BitXor);
    }

    fn emit_in(&mut self) {
        self.emit_opcode(Opcode::In);
    }

    fn emit_leftshift(&mut self) {
        self.emit_opcode(Opcode::LeftShift);
    }

    fn emit_rightshift(&mut self) {
        self.emit_opcode(Opcode::RightShift);
    }

    fn emit_unsigned_rightshift(&mut self) {
        self.emit_opcode(Opcode::UnsignedRightShift);
    }

    fn emit_instanceof(&mut self) {
        self.emit_opcode(Opcode::InstanceOf);
    }

    fn emit_and(&mut self, label: Label) {
        self.emit_opcode(Opcode::UnfixedAnd(label));
    }

    fn emit_or(&mut self, label: Label) {
        self.emit_opcode(Opcode::UnfixedOr(label));
    }

    fn emit_eq(&mut self) {
        self.emit_opcode(Opcode::Eq);
    }

    fn emit_stricteq(&mut self) {
        self.emit_opcode(Opcode::StrictEq);
    }

    fn emit_greaterthan(&mut self) {
        self.emit_opcode(Opcode::GreaterThan);
    }

    fn emit_greaterthan_eq(&mut self) {
        self.emit_opcode(Opcode::GreaterThanEq);
    }

    fn emit_delete_property(&mut self, name: InternedString) {
        self.emit_opcode(Opcode::DeleteProperty(name));
    }

    fn emit_delete_element(&mut self) {
        self.emit_opcode(Opcode::DeleteElement);
    }

    fn emit_delete_name(&mut self, name: InternedString) {
        self.emit_opcode(Opcode::DeleteName(name));
    }

    fn emit_get_property(&mut self, name: InternedString) {
        self.emit_opcode(Opcode::GetProperty(name));
    }

    fn emit_get_element(&mut self) {
        self.emit_opcode(Opcode::GetElement);
    }

    fn emit_put_property(&mut self, name: InternedString) {
        self.emit_opcode(Opcode::PutProperty(name));
    }

    fn emit_put_element(&mut self) {
        self.emit_opcode(Opcode::PutElement);
    }

    fn emit_init_property(&mut self, name: InternedString) {
        self.emit_opcode(Opcode::InitProperty(name));
    }

    fn emit_init_property_getter(&mut self, name: InternedString) {
        self.emit_opcode(Opcode::InitPropertyGetter(name));
    }

    fn emit_init_property_setter(&mut self, name: InternedString) {
        self.emit_opcode(Opcode::InitPropertySetter(name));
    }

    fn emit_enterwith(&mut self) {
        self.emit_opcode(Opcode::EnterWith);
    }

    fn emit_exitwith(&mut self) {
        self.emit_opcode(Opcode::ExitWith);
    }

    fn emit_ldname(&mut self, name: InternedString) {
        self.emit_opcode(Opcode::LdName(name));
    }

    fn emit_stname(&mut self, name: InternedString) {
        self.emit_opcode(Opcode::StName(name));
    }

    fn emit_brtrue(&mut self, label: Label) {
        self.emit_opcode(Opcode::UnfixedBrTrue(label));
    }

    fn emit_brfalse(&mut self, label: Label) {
        self.emit_opcode(Opcode::UnfixedBrFalse(label));
    }

    fn emit_jump(&mut self, label: Label) {
        self.emit_opcode(Opcode::UnfixedJump(label));
    }

    fn emit_debugger(&mut self) {
        self.emit_opcode(Opcode::Debugger);
    }

    fn emit_ldnum(&mut self, immediate: f64) {
        self.emit_opcode(Opcode::LdNum(immediate))
    }

    fn emit_ldbool(&mut self, immediate: bool) {
        self.emit_opcode(Opcode::LdBool(immediate))
    }

    fn emit_ldstring(&mut self, immediate: InternedString) {
        self.emit_opcode(Opcode::LdString(immediate));
    }

    fn emit_ldnull(&mut self) {
        self.emit_opcode(Opcode::LdNull);
    }

    fn emit_ldundefined(&mut self) {
        self.emit_opcode(Opcode::LdUndefined);
    }

    fn emit_ldregex(&mut self, reg: InternedString, flags: InternedString) {
        self.emit_opcode(Opcode::LdRegex(reg, flags));
    }

    fn emit_ldlambda(&mut self, idx: usize) {
        self.emit_opcode(Opcode::LdLambda(idx));
    }

    fn emit_ldobject(&mut self) {
        self.emit_opcode(Opcode::LdObject);
    }

    fn emit_ret(&mut self) {
        self.emit_opcode(Opcode::Ret);
    }

    fn emit_throw(&mut self) {
        self.emit_opcode(Opcode::Throw);
    }

    fn emit_call(&mut self, args: usize) {
        self.emit_opcode(Opcode::Call(args));
    }

    fn emit_new(&mut self, args: usize) {
        self.emit_opcode(Opcode::New(args));
    }

    fn emit_this(&mut self) {
        self.emit_opcode(Opcode::This);
    }

    fn emit_def(&mut self, name: InternedString) {
        self.emit_opcode(Opcode::Def(name))
    }

    fn emit_not_implemented(&mut self, value: &'static str) {
        self.emit_opcode(Opcode::NotImplemented(value))
    }
}

pub struct GlobalEmitter {
    global_opcodes: Vec<Opcode>,
    label_table: Vec<usize>,
}

impl BytecodeEmitter for GlobalEmitter {
    fn emit_opcode(&mut self, opcode: Opcode) {
        self.global_opcodes.push(opcode);
    }

    /// Makes room for an entry in the label table and returns
    /// that index.
    fn create_label(&mut self) -> Label {
        self.label_table.push(0);
        self.label_table.len() - 1
    }

    /// Marks a label with a given offset in the label table.
    fn mark_label(&mut self, label: Label) {
        let value = &mut self.label_table[label];
        *value = self.global_opcodes.len() - 1;
    }
}

impl GlobalEmitter {
    pub fn new() -> GlobalEmitter {
        GlobalEmitter {
            global_opcodes: vec![],
            label_table: vec![],
        }
    }

    pub fn bake(mut self,
                functions: Vec<CompiledFunction>,
                interner: StringInterner)
                -> CompiledProgram {
        fixup_labels(&mut self.global_opcodes, &self.label_table);
        CompiledProgram {
            strings: interner,
            functions: functions.into_boxed_slice(),
            global_functions: vec![self.global_opcodes.into_boxed_slice()],
        }
    }
}

pub struct FunctionEmitter {
    arity: usize,
    index: usize,
    opcodes: Vec<Opcode>,
    name: Option<InternedString>,
    label_table: Vec<usize>,
}

impl BytecodeEmitter for FunctionEmitter {
    fn emit_opcode(&mut self, opcode: Opcode) {
        self.opcodes.push(opcode);
    }

    /// Makes room for an entry in the label table and returns
    /// that index.
    fn create_label(&mut self) -> Label {
        self.label_table.push(0);
        self.label_table.len() - 1
    }

    /// Marks a label with a given offset in the label table.
    fn mark_label(&mut self, label: Label) {
        let value = &mut self.label_table[label];
        *value = self.opcodes.len() - 1;
    }
}

impl FunctionEmitter {
    pub fn new(index: usize) -> FunctionEmitter {
        FunctionEmitter {
            arity: 0,
            index: index,
            opcodes: vec![],
            name: None,
            label_table: vec![],
        }
    }

    pub fn set_arity(&mut self, arity: usize) {
        self.arity = arity;
    }

    pub fn set_name(&mut self, name: Option<InternedString>) {
        self.name = name;
    }

    pub fn bake(mut self) -> CompiledFunction {
        fixup_labels(&mut self.opcodes, &self.label_table);

        let compiled = CompiledFunction {
            name: self.name,
            arity: self.arity,
            code: self.opcodes.into_boxed_slice(),
        };

        compiled
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

/// In the bytecode emission stage, no `brtrue`, `brfalse`, or `jump` opcodes
/// are emitted directly - instead, a special unfixed version of each one of
/// those opcodes is emitted with an argument that is an index into this
/// function's label table. The objective of this function is to convert
/// these unfixed opcodes into their "fixed" versions - that is, `brtrue`,
/// `brfalse`, and `jump` opcodes with actual offsets encoded instead of
/// label table indexes.
fn fixup_labels(opcodes: &mut [Opcode], label_table: &[usize]) {
    for (i, opcode) in opcodes.iter_mut().enumerate() {
        let fixed_offset = match *opcode {
            Opcode::UnfixedAnd(index) |
            Opcode::UnfixedOr(index) |
            Opcode::UnfixedBrTrue(index) |
            Opcode::UnfixedBrFalse(index) |
            Opcode::UnfixedJump(index) => {
                let label = label_table[index];
                // this label is the absolute index of the target of
                // this branch. This needs to be converted into an offset.
                (label as isize) - (i as isize)
            }
            // not every opcode needs to be fixed up, so skip those.
            _ => continue,
        };

        *opcode = match *opcode {
            Opcode::UnfixedAnd(_) => Opcode::And(fixed_offset),
            Opcode::UnfixedOr(_) => Opcode::Or(fixed_offset),
            Opcode::UnfixedBrTrue(_) => Opcode::BrTrue(fixed_offset),
            Opcode::UnfixedBrFalse(_) => Opcode::BrFalse(fixed_offset),
            Opcode::UnfixedJump(_) => Opcode::Jump(fixed_offset),
            _ => unreachable!(),
        };
    }
}
