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
    assert!(atom(T));
    assert!(atom(NIL));
    assert!(!atom(SExpression::List(List::new(T, T))));
}

#[test]
fn test_eq() {
    assert!(eq(Atom::Number(1.), Atom::Number(1.)));
    assert!(eq(Atom::Symbol(Symbol::T), Atom::Symbol(Symbol::T)));
    assert!(!eq(Atom::Number(1.), Atom::Number(2.)));
    assert!(!eq(Atom::Symbol(Symbol::T), Atom::Symbol(Symbol::Nil)));
    assert!(!eq(Atom::Number(1.), Atom::Symbol(Symbol::Nil)));
}

#[test]
fn test_car() {
    assert_eq!(car(List::new(T, NIL)), T);
}

#[test]
fn test_cdr() {
    assert_eq!(cdr(List::new(T, NIL)), NIL);
}

#[test]
fn test_cons_car_cdr() {
    // same tests as above but using cons for constructing lists
    assert_eq!(car(cons(T, NIL)), T);

    assert_eq!(cdr(cons(T, NIL)), NIL);

    let x: List = cons(T, NIL);
    let expr: List = cons(car(x.clone()), cdr(x.clone()));

    assert_eq!(x, expr);
}
