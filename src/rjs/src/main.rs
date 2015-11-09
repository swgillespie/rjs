extern crate librjs;

use std::io::{self, Read, Write, BufRead, StdoutLock};
use std::fs::File;
use std::env;
use librjs::syntax::{Lexer, Parser};

fn main() {
    let args : Vec<_> = env::args().collect();
    if args.len() == 2 {
        parse_file(&args[1]);
        return;
    }

    let stdin_unlocked = io::stdin();
    let stdout_unlocked = io::stdout();
    let mut stdin = stdin_unlocked.lock();
    let mut stdout = stdout_unlocked.lock();
    loop {
        let mut buffer = String::new();
        write!(&mut stdout, ">> ").unwrap();
        stdout.flush().unwrap();
        stdin.read_line(&mut buffer).unwrap();
        parse_and_print(&buffer, &mut stdout);
    }
}

fn parse_and_print(data: &str, stdout: &mut StdoutLock) {
    let lexer = Lexer::new(data.chars());
    let mut parser = Parser::new(lexer);
    match parser.parse_statement() {
        Ok(a) => writeln!(stdout, "{:#?}", a).unwrap(),
        Err(e) => writeln!(stdout, "error: {:#?}", e).unwrap()
    }
    stdout.flush().unwrap();
}

fn parse_file(filename: &str) {
    let mut file = File::open(filename).unwrap();
    let mut file_contents = String::new();
    let _ = file.read_to_string(&mut file_contents).unwrap();
    let mut parser = Parser::new(Lexer::new(file_contents.chars()));
    match parser.parse_program() {
        Ok(_) => println!("parse suceeded! :-)"),
        Err(e) => println!("parse failed: {:#?}", e)
    }
}
