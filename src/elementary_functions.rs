use crate::types::*;

pub fn atom(x: SExpression) -> bool {
    match x {
        SExpression::Atom(_) => true,
        _ => false,
    }
}

pub fn eq(x: Atom, y: Atom) -> bool {
    x == y
}

pub fn car(x: List) -> SExpression {
    *x.0
}

pub fn cdr(x: List) -> SExpression {
    *x.1
}

pub fn cons(head: SExpression, tail: SExpression) -> List {
    List(Box::new(head), Box::new(tail))
}

#[test]
fn test_atom() {
    assert!(atom(T.into()));
    assert!(atom(NIL.into()));
    assert!(!atom(SExpression::List(List::new(T.into(), T.into()))));
}

#[test]
fn test_eq() {
    assert!(eq(1.into(), 1.into()));
    assert!(eq(T.into(), T.into()));
    assert!(!eq(1.into(), 2.into()));
    assert!(!eq(T.into(), NIL.into()));
    assert!(!eq(1.into(), NIL.into()));
}

#[test]
fn test_car() {
    assert_eq!(car(List::new(T.into(), NIL.into())), T.into());
}

#[test]
fn test_cdr() {
    assert_eq!(cdr(List::new(T.into(), NIL.into())), NIL.into());
}

#[test]
fn test_cons_car_cdr() {
    // same tests as above but using cons for constructing lists
    assert_eq!(car(cons(T.into(), NIL.into())), T.into());

    assert_eq!(cdr(cons(T.into(), NIL.into())), NIL.into());

    let x: List = cons(T.into(), NIL.into());
    let expr: List = cons(car(x.clone()), cdr(x.clone()));

    assert_eq!(x, expr);
}
