use librjs::runtime::compiler::{emitter, string_interer};
use librjs::runtime::compiler::bytecode::{self, Opcode};

pub fn disassemble_program(compiled: &emitter::CompiledProgram) {
    println!("disassembling program");
    for function in compiled.functions() {
        disassemble_function(function, compiled.interner());
    }
}

pub fn disassemble_function(function: &emitter::CompiledFunction,
                            interner: &string_interer::StringInterner) {
    let name = if let Some(name) = function.name() {
        interner.get(name)
    } else {
        "<anonymous function>"
    };

    println!("function \"{}\":", name);
    println!("  arity: {}", function.arity());
    println!("  code:");
    for op in function.code() {
        print_op(*op, interner);
    }

    println!("");
}

pub fn print_op(op: bytecode::Opcode, interner: &string_interer::StringInterner) {
    match op {
        Opcode::Nop => println!("    nop"),
        Opcode::Dup => println!("    dup"),
        Opcode::Pop => println!("    pop"),
        Opcode::Rotate => println!("    rotate"),
        Opcode::Neg => println!("    neg"),
        Opcode::Pos => println!("    pos"),
        Opcode::Not => println!("    not"),
        Opcode::BitNot => println!("    bitnot"),
        Opcode::Typeof => println!("    typeof"),
        Opcode::Add => println!("    add"),
        Opcode::Sub => println!("    sub"),
        Opcode::Mul => println!("    mul"),
        Opcode::Div => println!("    div"),
        Opcode::Mod => println!("    mod"),
        Opcode::BitOr => println!("    bitor"),
        Opcode::BitAnd => println!("    bitand"),
        Opcode::BitXor => println!("    bitxor"),
        Opcode::In => println!("    in"),
        Opcode::LeftShift => println!("    lshift"),
        Opcode::RightShift => println!("    rshift"),
        Opcode::UnsignedRightShift => println!("    urshift"),
        Opcode::InstanceOf => println!("    instanceof"),
        Opcode::And => println!("    and"),
        Opcode::Or => println!("    or"),
        Opcode::Eq => println!("    eq"),
        Opcode::StrictEq => println!("    stricteq"),
        Opcode::GreaterThan => println!("    gt"),
        Opcode::GreaterThanEq => println!("    geq"),
        Opcode::DeleteProperty(s) => println!("    delprop {}", interner.get(s)),
        Opcode::DeleteElement => println!("    delelem"),
        Opcode::DeleteName(s) => println!("    delname {}", interner.get(s)),
        Opcode::GetProperty(s) => println!("    getprop {}", interner.get(s)),
        Opcode::GetElement => println!("    getelem"),
        Opcode::PutProperty(s) => println!("    putprop {}", interner.get(s)),
        Opcode::PutElement => println!("    putelem"),
        Opcode::EnterWith => println!("    enterwith"),
        Opcode::ExitWith => println!("    exitwith"),
        Opcode::LdName(s) => println!("    ldname {}", interner.get(s)),
        Opcode::StName(s) => println!("    stname {}", interner.get(s)),
        Opcode::BrTrue(o) => println!("    brtrue {}", o),
        Opcode::BrFalse(o) => println!("    brfalse {}", o),
        Opcode::Jump(o) => println!("    jump {}", o),
        Opcode::Debugger => println!("    debugger"),
        Opcode::LdNum(f) => println!("    ldnum {}", f),
        Opcode::LdBool(b) => println!("    ldbool {}", b),
        Opcode::LdString(s) => println!("    ldstr \"{}\"", interner.get(s)),
        Opcode::LdNull => println!("    ldnull"),
        Opcode::LdUndefined => println!("    ldundefined"),
        Opcode::LdRegex(a, s) => println!("    ldlregex /{}/{}", interner.get(a), interner.get(s)),
        Opcode::LdLambda(i) => println!("    ldlambda <anonymous function #{}>", i),
        Opcode::Ret => println!("    ret"),
        Opcode::Throw => println!("    throw"),
        Opcode::Def(s) => println!("    def {}", interner.get(s)),
        Opcode::This => println!("    this"),
        Opcode::Call(i) => println!("    call {}", i),
        Opcode::NotImplemented(s) => println!("    not_implemented {}", s),
        e => panic!("invalid opcode: {:?}", e),
    }
}
