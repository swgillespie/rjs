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
use librjs::runtime::values::Value;
use time::Duration;
use std::ffi::CString;
use std::default::Default;

fn main() {
    env_logger::init().unwrap();
    let args: Vec<_> = env::args().collect();

    let mut ee = ExecutionEngine::new(Default::default());

    // expose a printing function
    ee.expose_function("print", |ee, _, arguments| {
        for (i, arg) in arguments.iter().enumerate() {
            if i != 0 {
                print!(" ");
            }
            let s = librjs::runtime::exec::helpers::to_string(ee, arg);
            print!("{}", s);
        }

        println!("");
        return Ok(ee.heap_mut().root_value(Value::undefined()));
    });

    // expose that same function as $ERROR for use with test262
    ee.expose_function("$ERROR", |ee, _, arguments| {
        for (i, arg) in arguments.iter().enumerate() {
            if i != 0 {
                print!(" ");
            }
            let s = librjs::runtime::exec::helpers::to_string(ee, arg);
            print!("{}", s);
        }

        println!("");
        return Ok(ee.heap_mut().root_value(Value::undefined()));
    });

    if args.len() == 2 {
        exec_file(&mut ee, &args[1]);
        return;
    }


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

        dis::disassemble_program(ee.program());
    }
}

fn exec_file(ee: &mut ExecutionEngine, filename: &str) {
    let mut file = File::open(filename).unwrap();
    let mut file_contents = String::new();
    let _ = file.read_to_string(&mut file_contents).unwrap();
    let mut result = None;
    let _ = Duration::span(|| {
        match ee.eval_str(file_contents) {
            Ok(value) => {
                result = Some(librjs::runtime::exec::helpers::to_string(ee, &value));
            },
            Err(e) => panic!("error: {:?}", e)
        };
    });

    // println!("{}", result.unwrap());
    // println!("");
    // println!("  exec time:       {} ms ({} μs)",
    // exec_time.num_milliseconds(),
    // exec_time.num_microseconds().unwrap_or_default());
    // println!("");
    // dis::disassemble_program(ee.program());
}
