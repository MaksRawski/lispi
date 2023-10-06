use crate::types::*;

pub fn atom(x: SExpression) -> bool {
    matches!(x, SExpression::Atom(_))
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

pub fn cons<H, T>(head: H, tail: T) -> List
where
    H: Into<SExpression>,
    T: Into<SExpression>,
{
    List(Box::new(head.into()), Box::new(tail.into()))
}

#[test]
fn test_atom() {
    assert!(atom(T.into()));
    assert!(atom(F.into()));
    assert!(atom(NIL.into()));
    assert!(!atom(SExpression::List(List::new(T.into(), T.into()))));
}

#[test]
fn test_eq() {
    assert!(eq(1.into(), 1.into()));
    assert!(eq(T.into(), T.into()));
    assert!(eq("X".into(), "X".into()));
    assert!(!eq(1.into(), 2.into()));
    assert!(!eq(T.into(), NIL));
    assert!(!eq(1.into(), NIL));
    assert!(!eq("X".into(), NIL));
}

#[test]
fn test_car() {
    assert_eq!(car(List::new("A".into(), NIL.into())), "A".into());
    assert_eq!(car(List::new("A".into(), "B".into())), "A".into());
    assert_eq!(
        car(List::new(
            List::new("A".into(), "B".into()).into(),
            "C".into()
        )),
        List::new("A".into(), "B".into()).into()
    );
}

#[test]
fn test_cdr() {
    assert_eq!(cdr(List::new("A".into(), NIL.into())), NIL.into());
    assert_eq!(cdr(List::new("A".into(), "B".into())), "B".into());
    assert_eq!(
        cdr(List::new(
            "A".into(),
            List::new("B".into(), "C".into()).into()
        )),
        List::new("B".into(), "C".into()).into()
    );
}

#[test]
fn test_cons_car_cdr() {
    // same tests as above but using cons for constructing lists
    assert_eq!(car(cons(T, NIL)), T.into());

    assert_eq!(cdr(cons(T, NIL)), NIL.into());

    let x: List = cons(T, NIL);
    let expr: List = cons(car(x.clone()), cdr(x.clone()));

    assert_eq!(x, expr);
}
