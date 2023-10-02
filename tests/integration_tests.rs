use lisp_interpreter::parser::parse;

#[test]
fn test_parse_eval_ff() {
    let text = "((label ff (lambda (x) (cond ((atom x) x) (T (ff (car x)))))) (cons 1 2))";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval()), Some(1.into()))
}

#[test]
fn test_parse_eval_car_cons() {
    let text = "(car (cons 1 2))";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval()), Some(1.into()));

    let text = "(car (cons (quote A) (quote B)))";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval()), Some("A".into()));
}

#[test]
fn test_parse_eval_cond() {
    use lisp_interpreter::types::Atom;

    let text = "(cond ((atom 1) \"OK\") (T \"ERROR\"))";
    let prog = parse(text).unwrap();
    assert_eq!(
        dbg!(dbg!(prog).eval()),
        Some(Atom::String("OK".to_string()).into())
    );

    let text = "(cond ((atom (cons 1 2)) \"ERROR\") (T \"OK\"))";
    let prog = parse(text).unwrap();
    assert_eq!(
        dbg!(dbg!(prog).eval()),
        Some(Atom::String("OK".to_string()).into())
    );
}
