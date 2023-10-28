use lispi::{file_parser::eval_file, types::NIL};

#[test]
fn test_eval_file_peano_fac_list() {
    let mut output = Vec::with_capacity(10);
    let expected_output = format!(
        "s\npeano\np\nadd\nmul\nfac\nseq\nmaplist\ncount\nfac1\n{}\n{}\n",
        "[0, 1, 2, 3, 4]", "[1, 1, 2, 6, 24]"
    );
    assert_eq!(
        eval_file("peano.lisp", &mut NIL.into(), &mut output),
        Some(())
    );
    assert_eq!(String::from_utf8(output).unwrap(), expected_output);
}
