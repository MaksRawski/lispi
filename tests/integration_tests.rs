use lispi::parser::parse;

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
    let text = "(cond ((atom 1) (quote OK)) (T (quote ERROR)))";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval()), Some("OK".into()));

    let text = "(cond ((atom (cons 1 2)) (quote ERROR)) (T (quote OK)))";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval()), Some("OK".into()));
}

#[test]
fn test_parse_eval_nums() {
    env_logger::init();
    let text = "(sum 1.23 2.34)";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval()), Some(3.57.into()));

    let text = "(sum (prdct 1.25 4) 2.5)";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval()), Some(7.5.into()));
}

#[test]
fn test_parse_eval_fac() {
    let text = "((label fac (lambda (n) (cond ((equal n 0) 1) (T (prdct n (fac (sum n -1))))))) 6)";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval()), Some(720.into()));
}
