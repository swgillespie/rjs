extern crate librjs;
extern crate time;

use std::io::{self, Read, Write, BufRead, StdoutLock};
use std::fs::File;
use std::env;
use librjs::syntax::{Lexer, Parser};
use librjs::runtime::compiler::{self, string_interer};
use time::Duration;

fn main() {
    let args : Vec<_> = env::args().collect();
    if args.len() == 2 {
        parse_file(&args[1]);
        return;
    }

    let mut interner = string_interer::StringInterner::new();
    let stdin_unlocked = io::stdin();
    let stdout_unlocked = io::stdout();
    let mut stdin = stdin_unlocked.lock();
    let mut stdout = stdout_unlocked.lock();
    loop {
        let mut buffer = String::new();
        write!(&mut stdout, ">> ").unwrap();
        stdout.flush().unwrap();
        stdin.read_line(&mut buffer).unwrap();
        parse_and_print(&buffer, &mut stdout, &mut interner);
    }
}

fn parse_and_print(data: &str, stdout: &mut StdoutLock, interner: &mut string_interer::StringInterner) {
    let lexer = Lexer::new(data.chars());
    let mut parser = Parser::new(lexer);
    let ast = match parser.parse_statement() {
        Ok(a) => a,
        Err(e) => {
            writeln!(stdout, "error: {:#?}", e).unwrap();
            return;
        }
    };

    let hir = compiler::lower_statement_to_hir(interner, &ast);
    writeln!(stdout, "{:#?}", hir).unwrap();

    stdout.flush().unwrap();
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

    let real_ast = ast.unwrap();

    let mut interner = string_interer::StringInterner::new();
    let hir_time = Duration::span(|| {
        compiler::lower_program_to_hir(&mut interner, &real_ast);
    });

    println!("hir construction complete");
    println!("  parse time:       {} ms", parse_time.num_milliseconds());
    println!("  hir construction: {} ms", hir_time.num_milliseconds());
}
