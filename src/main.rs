use std::io;
use std::io::Write;
use std::process::exit;

use clap::{App, Arg};
use lispi::file_parser::get_bound_symbols;
use lispi::repl::MyHelper;
use lispi::{
    file_parser::eval_file,
    interpreter::eval,
    parser::parse,
    types::{NullableList, NIL},
};
use rustyline::history::MemHistory;
use rustyline::Config;
use rustyline::{error::ReadlineError, Editor};

fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Info)
        .format(|buf, record| {
            writeln!(
                buf,
                "{}: {}",
                buf.default_styled_level(record.level()),
                record.args(),
            )
        })
        .init();

    let args = App::new("lispi")
        .version("0.3.2")
        .arg(Arg::with_name("FILE").help("File to evaluate"))
        .arg(
            Arg::with_name("load_file")
                .conflicts_with("FILE")
                .short("l")
                .long("load")
                .takes_value(true)
                .value_name("FILE")
                .help("Loads the content of FILE into the REPL"),
        )
        .get_matches();

    let mut a: NullableList = NIL.into();
    if let Some(filename) = args.value_of("FILE") {
        if eval_file(filename, &mut io::stdout()).is_some() {
            exit(0);
        } else {
            exit(1);
        }
    } else if let Some(load_file) = args.value_of("load_file") {
        if let Some(a_list) = eval_file(load_file, &mut Vec::new()) {
            a = a_list;
        }
        if let Some(symbols) = get_bound_symbols(&a) {
            print!("Loaded symbols: ");
            for symbol in symbols {
                print!("{} ", symbol);
            }
            println!();
        }
    }

    let mut rl = Editor::with_history(Config::default(), MemHistory::default()).unwrap();
    rl.set_helper(Some(MyHelper::new()));

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).unwrap();
                match parse(&line) {
                    Some(prog) => match eval(prog, &a) {
                        Some((result, new_a_list)) => {
                            a = new_a_list.into_owned();
                            println!("{}", result)
                        }
                        None => {
                            continue;
                        }
                    },
                    None => continue,
                }
            }
            Err(ReadlineError::Interrupted) => {
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
