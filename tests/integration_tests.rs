use lispi::{file_parser::eval_file, types::NIL};

#[test]
fn test_eval_file() {
    let mut output = Vec::with_capacity(10);
    let expected_output = format!("{}{}", "NIL\n".repeat(7), "24\n");
    assert_eq!(
        eval_file("peano.lisp", &mut NIL.into(), &mut output),
        Some(())
    );
    assert_eq!(String::from_utf8(output).unwrap(), expected_output);
}
