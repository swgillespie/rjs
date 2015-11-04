extern crate librjs;

use std::io::{self, Read, Write, BufRead, StdoutLock};
use librjs::syntax::{Lexer, Parser};

fn main() {
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
