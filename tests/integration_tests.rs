use lispi::{interpreter::eval, parser::parse, types::NIL};

#[test]
fn test_parse_eval_ff() {
    let text = "((label ff (lambda (x) (cond ((atom x) x) (T (ff (car x)))))) (cons 1 2))";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval(NIL.into())), Some(1.into()))
}

#[test]
fn test_parse_eval_car_cons() {
    let text = "(car (cons 1 2))";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval(NIL.into())), Some(1.into()));

    let text = "(car (cons (quote A) (quote B)))";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval(NIL.into())), Some("A".into()));
}

#[test]
fn test_parse_eval_cond() {
    let text = "(cond ((atom 1) (quote OK)) (T (quote ERROR)))";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval(NIL.into())), Some("OK".into()));

    let text = "(cond ((atom (cons 1 2)) (quote ERROR)) (T (quote OK)))";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval(NIL.into())), Some("OK".into()));
}

#[test]
fn test_parse_eval_nums() {
    let text = "(sum 1.23 2.34)";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval(NIL.into())), Some(3.57.into()));

    let text = "(sum (prdct 1.25 4) 2.5)";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval(NIL.into())), Some(7.5.into()));
}

#[test]
fn test_parse_eval_fac() {
    let text = "((label fac (lambda (n) (cond ((equal n 0) 1) (T (prdct n (fac (sum n -1))))))) 6)";
    let prog = parse(text).unwrap();
    assert_eq!(dbg!(dbg!(prog).eval(NIL.into())), Some(720.into()));
}

#[test]
fn test_parse_eval_define_fac() {
    env_logger::init();
    let fac = "(define (fac (lambda (n) (cond ((equal n 0) 1) (T (prdct n (fac (sum n -1))))))))";
    let fac_def = parse(fac).unwrap();
    let (_, a_list) = eval(fac_def, NIL.into()).unwrap();
    let fac_test = parse("(fac 6)").unwrap();

    assert_eq!(dbg!(dbg!(fac_test).eval(a_list)), Some(720.into()));
}
#[test]
fn test_parse_eval_define_ff() {
    let ff = "(define (ff (lambda (x) (cond ((atom x) x) (T (ff (car x)))))))";
    let ff_def = parse(ff).unwrap();
    let (_, a_list) = eval(ff_def, NIL.into()).unwrap();
    let ff_test = parse("(ff (quote ((a b) c)))").unwrap();

    assert_eq!(dbg!(dbg!(ff_test).eval(a_list)), Some("a".into()));
}

#[test]
fn test_parse_eval_define_maplist() {
    use lispi::{list, types::T};
    env_logger::init();

    let maplist = "(define (maplist (lambda (f x)
(cond
      ((equal x NIL) NIL)
      ((atom x) (f x))
      (T (cons (f (car x)) (maplist f (cdr x))))))))";

    let maplist_def = parse(maplist).unwrap();
    let (_, a_list) = eval(maplist_def, NIL.into()).unwrap();
    // assert_eq!(
    //     parse("(maplist atom NIL)").unwrap().eval(a_list.clone()),
    //     Some(NIL.into())
    // );
    assert_eq!(
        parse("(maplist atom 1)").unwrap().eval(a_list.clone()),
        Some(T.into())
    );
    assert_eq!(
        parse("(maplist atom (quote (1 2 3)))")
            .unwrap()
            .eval(a_list.clone()),
        Some(list![T, T, T].into())
    );
    assert_eq!(
        parse("(maplist atom (quote (1 2 (3 4))))")
            .unwrap()
            .eval(a_list),
        Some(list![T, T, F].into())
    );
}
