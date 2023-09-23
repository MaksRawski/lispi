use crate::elementary_functions::*;
use crate::types::*;

/// the first atomic symbol of an S-Expression
pub fn ff(expr: SExpression) -> Atom {
    match expr {
        SExpression::Atom(a) => a,
        SExpression::List(l) => ff(car(l)),
    }
}

/// substitute with x all occurences of y in expression z
pub fn subst(x: SExpression, y: Atom, z: SExpression) -> SExpression {
    match z.clone() {
        SExpression::Atom(a) => {
            if a == y {
                x
            } else {
                z
            }
        }
        SExpression::List(l) => {
            SExpression::List(cons(
                subst(x.clone(), y.clone(), car(l.clone())),
                subst(x, y, cdr(l)),
            ))
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
    assert_eq!(ff(cons(cons("A", "B"), "C").into()), "A".into());
}

#[test]
fn test_subst() {
    assert_eq!(
        subst("A".into(), 1.into(), cons(1, 2).into()),
        cons("A", 2).into()
    );
    assert_eq!(
        subst("A".into(), 1.into(), cons(cons(1, 2), 1).into()),
        cons(cons("A", 2), "A").into()
    );
}

#[test]
fn test_equal() {
    let x: SExpression = cons(cons(T, T), NIL).into();
    let y: SExpression = cons(cons(T, NIL), NIL).into();

    assert!(equal(x.clone(), x.clone()));
    assert!(!equal(x, y));
}

#[test]
fn test_null() {
    assert!(null(NIL.into()));
    assert!(!null(T.into()));
    assert!(!null(cons(NIL, NIL).into()));
}
