mod repl;
use lispi::{
    interpreter::eval,
    parser::parse,
    types::{NullableList, NIL},
};
use repl::MyHelper;
use rustyline::{error::ReadlineError, Editor};

fn main() {
    env_logger::builder()
        .format_timestamp(None)
        .format_target(false)
        .init();
    let mut rl = Editor::new().unwrap();
    let mut a: NullableList = NIL.into();
    rl.set_helper(Some(MyHelper::new()));

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).unwrap();
                match parse(&line) {
                    Some(prog) => match eval(prog, a.clone()) {
                        Some((result, new_a_list)) => {
                            a = new_a_list;
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
