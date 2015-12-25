extern crate librjs;
extern crate time;
extern crate readline;

mod dis;

use std::io::{self, Read, Write};
use std::fs::File;
use std::env;
use librjs::syntax::{Lexer, Parser};
use librjs::runtime::compiler;
use librjs::runtime::exec::engine::ExecutionEngine;
use time::Duration;
use std::ffi::CString;
use std::default::Default;

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() == 2 {
        parse_file(&args[1]);
        return;
    }

    let mut ee = ExecutionEngine::new(Default::default());
    let stdout_unlocked = io::stdout();
    let mut stdout = stdout_unlocked.lock();
    let prompt = CString::new(">> ").unwrap();
    while let Ok(data) = readline::readline(&prompt) {
        let as_str = match data.to_str() {
            Ok(s) => s,
            Err(_) => {
                writeln!(&mut stdout, "not valid UTF8").unwrap();
                continue;
            }
        };

        readline::add_history(&data);
        let result = match ee.eval_str(as_str) {
            Ok(value) => {
                librjs::runtime::exec::helpers::to_string(&mut ee, &value)
            },
            Err(e) => panic!("error: {:?}", e)
        };

        writeln!(&mut stdout, "{}", result).unwrap();
        stdout.flush().unwrap();
    }
}

fn parse_file(filename: &str) {
    let mut file = File::open(filename).unwrap();
    let mut file_contents = String::new();
    let _ = file.read_to_string(&mut file_contents).unwrap();
    let mut parser = Parser::new(Lexer::new(file_contents.chars()));
    let mut ast = None;
    let parse_time = Duration::span(|| {
        ast = match parser.parse_program() {
            Ok(a) => Some(a),
            Err(e) => {
                println!("parse failed: {:#?}", e);
                return;
            }
        };
    });

    let real_ast = if let Some(actual_ast) = ast {
        actual_ast
    } else {
        return;
    };

    let mut interner = compiler::StringInterner::new();
    let mut hir = None;
    let hir_time = Duration::span(|| {
        hir = Some(compiler::lower_program_to_hir(&mut interner, &real_ast));
    });

    let real_hir = hir.unwrap();

    // println!("{:#?}", real_hir);

    let mut compiled_program = None;
    let bytecode_time = Duration::span(|| {
        compiled_program = Some(compiler::lower_hir_to_bytecode(interner, &real_hir));
    });

    dis::disassemble_program(compiled_program.as_ref().unwrap());


    println!("hir construction complete");
    println!("  parse time:       {} ms ({} μs)",
             parse_time.num_milliseconds(),
             parse_time.num_microseconds().unwrap_or_default());
    println!("  hir construction: {} ms ({} μs)",
             hir_time.num_milliseconds(),
             hir_time.num_microseconds().unwrap_or_default());
    println!("  bytecode emit:    {} ms ({} μs)",
             bytecode_time.num_milliseconds(),
             bytecode_time.num_microseconds().unwrap_or_default());
}
