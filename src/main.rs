mod repl;
use lispi::parser::parse;
use repl::MyHelper;
use rustyline::{error::ReadlineError, Cmd, Editor};

fn main() {
    env_logger::builder()
        .format_timestamp(None)
        .format_target(false)
        .init();
    let mut rl = Editor::new().unwrap();
    rl.set_helper(Some(MyHelper::new()));

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).unwrap();
                match parse(&line) {
                    Some(prog) => match prog.eval() {
                        Some(result) => {
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
