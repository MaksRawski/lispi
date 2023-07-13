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

/// predicate which checks if S-Expression is basically Nil
pub fn null(x: SExpression) -> bool {
    match x {
        SExpression::Atom(a) => eq(a, Atom::Symbol(Symbol::Nil)),
        SExpression::List(_) => false,
    }
}

#[test]
fn test_ff() {
    const A: SExpression = SExpression::Atom(Atom::Number(1.));
    const B: SExpression = SExpression::Atom(Atom::Number(2.));
    const C: SExpression = SExpression::Atom(Atom::Number(3.));

    let ff_example_list = cons(cons(A, B).into(), C);
    dbg!(ff_example_list.clone());

    let ff_result = ff(ff_example_list.into());
    dbg!(ff_result.clone());

    assert_eq!(ff_result, Atom::Number(1.));
}

#[test]
fn test_subst_display() {
    // subst[(X · A); B; ((A · B) · C)] = ((A · (X · A)) · C)
    const X: SExpression = SExpression::Atom(Atom::Number(1.));
    const A: SExpression = NIL;
    const B: Atom = Atom::Symbol(Symbol::T);
    const C: SExpression = SExpression::Atom(Atom::Number(2.));

    let x: SExpression = cons(X, A).into();
    let y: Atom = B;
    let z: SExpression = cons(cons(A, B.into()).into(), C).into();

    // not bothering this time with manual checking and instead using
    // the Display implementation to check the human readable representation
    assert_eq!(
        format!("{}", subst(x, y, z)),
        format!("(({A} · ({X} · {A})) · {C})")
    );
}

#[test]
fn test_equal() {
    let x: SExpression = cons(cons(T, T).into(), NIL).into();
    let y: SExpression = cons(cons(T, NIL).into(), NIL).into();

    assert!(equal(x.clone(), x.clone()));
    assert!(!equal(x, y));
}

#[test]
fn test_null() {
    assert!(null(NIL));
    assert!(!null(T));
    assert!(!null(cons(NIL, NIL).into()));
}
