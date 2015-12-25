extern crate phf_codegen;

use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

fn main() {
    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("reserved_words.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());

    write!(&mut file,
           "static RESERVED_WORDS: phf::Map<&'static str, TokenKind> = ")
        .unwrap();
    phf_codegen::Map::new()
        .entry("break", "TokenKind::Break")
        .entry("do", "TokenKind::Do")
        .entry("instanceof", "TokenKind::InstanceOf")
        .entry("typeof", "TokenKind::TypeOf")
        .entry("case", "TokenKind::Case")
        .entry("else", "TokenKind::Else")
        .entry("new", "TokenKind::New")
        .entry("var", "TokenKind::Var")
        .entry("catch", "TokenKind::Catch")
        .entry("finally", "TokenKind::Finally")
        .entry("return", "TokenKind::Return")
        .entry("void", "TokenKind::Void")
        .entry("continue", "TokenKind::Continue")
        .entry("for", "TokenKind::For")
        .entry("switch", "TokenKind::Switch")
        .entry("while", "TokenKind::While")
        .entry("debugger", "TokenKind::Debugger")
        .entry("function", "TokenKind::Function")
        .entry("this", "TokenKind::This")
        .entry("with", "TokenKind::With")
        .entry("default", "TokenKind::Default")
        .entry("if", "TokenKind::If")
        .entry("throw", "TokenKind::Throw")
        .entry("delete", "TokenKind::Delete")
        .entry("in", "TokenKind::In")
        .entry("try", "TokenKind::Try")
        .entry("class", "TokenKind::Class")
        .entry("enum", "TokenKind::Enum")
        .entry("extends", "TokenKind::Extends")
        .entry("super", "TokenKind::Super")
        .entry("const", "TokenKind::Const")
        .entry("export", "TokenKind::Export")
        .entry("import", "TokenKind::Import")
        .entry("implements",
               "TokenKind::FutureReservedWordStrict(\"implements\")")
        .entry("let", "TokenKind::FutureReservedWordStrict(\"let\")")
        .entry("private",
               "TokenKind::FutureReservedWordStrict(\"private\")")
        .entry("public", "TokenKind::FutureReservedWordStrict(\"public\")")
        .entry("yield", "TokenKind::FutureReservedWordStrict(\"yield\")")
        .entry("interface",
               "TokenKind::FutureReservedWordStrict(\"interface\")")
        .entry("package",
               "TokenKind::FutureReservedWordStrict(\"package\")")
        .entry("protected",
               "TokenKind::FutureReservedWordStrict(\"protected\")")
        .entry("static", "TokenKind::FutureReservedWordStrict(\"static\")")
        .entry("null", "TokenKind::NullLiteral")
        .entry("false", "TokenKind::BooleanLiteral(false)")
        .entry("true", "TokenKind::BooleanLiteral(true)")
        .build(&mut file)
        .unwrap();
    write!(&mut file, ";\n").unwrap();
}
