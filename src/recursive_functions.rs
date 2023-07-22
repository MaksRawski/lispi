use crate::elementary_functions::*;
use crate::types::*;

/// the first atomic symbol of an S-Expression
pub fn ff(expr: SExpression) -> Atom {
    match expr {
        SExpression::Atom(a) => return a,
        SExpression::List(l) => return ff(car(l)),
    }
}

/// substitute with x all occurences of y in expression z
pub fn subst(x: SExpression, y: Atom, z: SExpression) -> SExpression {
    match z.clone() {
        SExpression::Atom(a) => {
            if a == y {
                return x;
            } else {
                return z;
            }
        }
        SExpression::List(l) => {
            return SExpression::List(cons(
                subst(x.clone(), y.clone(), car(l.clone())),
                subst(x, y, cdr(l)),
            ));
        }
    }
}

/// predicate which compares two S-Expressions
pub fn equal(x: SExpression, y: SExpression) -> bool {
    match x {
        SExpression::Atom(ax) => match y {
            SExpression::Atom(ay) => ax == ay,
            SExpression::List(_) => false,
        },
        SExpression::List(lx) => match y {
            SExpression::Atom(_) => false,
            SExpression::List(ly) => {
                equal(car(lx.clone()), car(ly.clone())) && equal(cdr(lx), cdr(ly))
            }
        },
    }
}

/// predicate which checks if S-Expression is NIL
pub fn null(x: SExpression) -> bool {
    match x {
        SExpression::Atom(a) => eq(a, NIL.into()),
        SExpression::List(_) => false,
    }
}

#[test]
fn test_ff() {
    assert_eq!(
        ff(cons(cons("A".into(), "B".into()).into(), "C".into()).into()),
        "A".into()
    );
}

#[test]
fn test_subst() {
    assert_eq!(
        subst("A".into(), 1.into(), cons(1.into(), 2.into()).into()),
        cons("A".into(), 2.into()).into()
    );
    assert_eq!(
        subst(
            "A".into(),
            1.into(),
            cons(cons(1.into(), 2.into()).into(), 1.into()).into()
        ),
        cons(cons("A".into(), 2.into()).into(), "A".into()).into()
    );
}

#[test]
fn test_equal() {
    let x: SExpression = cons(cons(T.into(), T.into()).into(), NIL.into()).into();
    let y: SExpression = cons(cons(T.into(), NIL.into()).into(), NIL.into()).into();

    assert!(equal(x.clone(), x.clone()));
    assert!(!equal(x, y));
}

#[test]
fn test_null() {
    assert!(null(NIL.into()));
    assert!(!null(T.into()));
    assert!(!null(cons(NIL.into(), NIL.into()).into()));
}
