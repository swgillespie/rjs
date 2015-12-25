#![allow(dead_code)]

extern crate librjs;
extern crate time;
extern crate readline;
extern crate env_logger;

mod dis;

use std::io::{self, Read, Write};
use std::fs::File;
use std::env;
use librjs::runtime::exec::engine::ExecutionEngine;
use time::Duration;
use std::ffi::CString;
use std::default::Default;

fn main() {
    env_logger::init().unwrap();
    let args: Vec<_> = env::args().collect();
    if args.len() == 2 {
        exec_file(&args[1]);
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

fn exec_file(filename: &str) {
    let mut file = File::open(filename).unwrap();
    let mut file_contents = String::new();
    let _ = file.read_to_string(&mut file_contents).unwrap();

    let mut ee = ExecutionEngine::new(Default::default());
    let mut result = None;
    let exec_time = Duration::span(|| {
        match ee.eval_str(file_contents) {
            Ok(value) => {
                result = Some(librjs::runtime::exec::helpers::to_string(&mut ee, &value));
            },
            Err(e) => panic!("error: {:?}", e)
        };
    });

    println!("{}", result.unwrap());
    println!("");
    println!("  exec time:       {} ms ({} μs)",
             exec_time.num_milliseconds(),
             exec_time.num_microseconds().unwrap_or_default());
    println!("");

    dis::disassemble_program(ee.program());
}
