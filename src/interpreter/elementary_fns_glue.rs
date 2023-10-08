use crate::list;

use crate::interpreter::eval;
use crate::{
    elementary_functions::*,
    list_macros::compose_car_cdr,
    types::{List, NullableList, SExpression},
};

pub(crate) fn atom_fn(e_list: List, a: NullableList) -> Option<SExpression> {
    match compose_car_cdr("cadr", e_list) {
        Some(arg) => Some(atom(eval(arg, a)?.0).into()),
        None => {
            log::error!("ATOM requires an argument.");
            None
        }
    }
}

pub(crate) fn car_fn(e_list: List, a: NullableList) -> Option<SExpression> {
    match eval(
        compose_car_cdr("cadr", e_list).or_else(|| {
            log::error!("CAR requires a list as argument.");
            None
        })?,
        a,
    )?
    .0
    {
        SExpression::List(list) => Some(car(list)),
        SExpression::Atom(argument) => {
            log::error!(
                "CAR requires an argument to be a list, but was: {}",
                argument
            );
            None
        }
    }
}

pub(crate) fn cdr_fn(e_list: List, a: NullableList) -> Option<SExpression> {
    match eval(
        compose_car_cdr("cadr", e_list).or_else(|| {
            log::error!("CDR requires a list as argument.");
            None
        })?,
        a,
    )?
    .0
    {
        SExpression::List(list) => Some(cdr(list)),
        SExpression::Atom(argument) => {
            log::error!(
                "CDR requires an argument to be a list, but was: {}",
                argument
            );
            None
        }
    }
}

pub(crate) fn cons_fn(e_list: List, a: NullableList) -> Option<SExpression> {
    match cdr(e_list.clone()) {
        SExpression::List(arguments) => Some(
            list![
                eval(car(arguments.clone()), a.clone())?.0,
                eval(
                    compose_car_cdr("cadr", arguments).or_else(|| {
                        log::error!(
                            "CONS requires two arguments, but only one was provided: {}",
                            e_list
                        );
                        None
                    })?,
                    a
                )?
                .0
            ]
            .into(),
        ),
        SExpression::Atom(_argument) => {
            log::error!(
                "Invalid use of CONS: {}, two arguments are required.",
                e_list
            );
            None
        }
    }
}

pub(crate) fn eq_fn(e_list: List, a: NullableList) -> Option<SExpression> {
    match cdr(e_list.clone()) {
        SExpression::List(arguments) => {
            let x = eval(car(arguments.clone()), a.clone())?.0;
            let y = eval(
                compose_car_cdr("cadr", arguments).or_else(|| {
                    log::error!(
                        "EQ requires two arguments, but only one was provided: {}",
                        e_list
                    );
                    None
                })?,
                a,
            )?
            .0;
            if let SExpression::Atom(ax) = x {
                if let SExpression::Atom(ay) = y {
                    Some(eq(ax, ay).into())
                } else {
                    log::error!(
                        "EQ requires its arguments to be atoms, use EQUAL to compare lists: {}",
                        e_list
                    );
                    None
                }
            } else {
                log::error!(
                    "EQ requires its arguments to be atoms, use EQUAL to compare lists: {}",
                    e_list
                );
                None
            }
        }
        SExpression::Atom(_) => {
            log::error!("Invalid use of EQ: {} two arguments are required.", e_list);
            None
        }
    }
}

#[test]
fn test_atom_fn() {
    use crate::types::{NIL, T};
    assert_eq!(
        atom_fn(
            list![ElementaryFunction::ATOM, list![SpecialForm::QUOTE, "x"]],
            NIL.into()
        ),
        Some(T.into())
    );
    assert_eq!(atom_fn(list![ElementaryFunction::ATOM], NIL.into()), None);
}

#[test]
fn test_car_fn() {
    use crate::types::NIL;
    assert_eq!(
        car_fn(
            list![
                ElementaryFunction::CAR,
                list![SpecialForm::QUOTE, list!["A", "B", "C"]]
            ],
            NIL.into(),
        ),
        Some("A".into())
    );
    assert_eq!(
        car_fn(
            list![ElementaryFunction::CAR, list![SpecialForm::QUOTE, "A"]],
            NIL.into(),
        ),
        None
    );
    assert_eq!(car_fn(list![ElementaryFunction::CAR], NIL.into(),), None);
}

#[test]
fn test_cdr_fn() {
    use crate::types::NIL;
    assert_eq!(
        cdr_fn(
            list![
                ElementaryFunction::CDR,
                list![SpecialForm::QUOTE, list!["A", "B", "C"]]
            ],
            NIL.into(),
        ),
        Some(list!["B", "C"].into())
    );
    assert_eq!(
        cdr_fn(
            list![
                ElementaryFunction::CDR,
                list![SpecialForm::QUOTE, list!["A", "B"]]
            ],
            NIL.into(),
        ),
        Some(list!["B"].into())
    );
    assert_eq!(
        cdr_fn(list![ElementaryFunction::CDR, "A"], NIL.into()),
        None
    );
    assert_eq!(cdr_fn(list![ElementaryFunction::CDR], NIL.into()), None);
}

#[test]
fn test_cons_fn() {
    use crate::types::NIL;
    assert_eq!(
        cons_fn(list![ElementaryFunction::CONS, "A", "B"], NIL.into()),
        Some(list!["A", "B"].into())
    );
    assert_eq!(
        cons_fn(list![ElementaryFunction::CONS, "A"], NIL.into()),
        None
    );
    assert_eq!(cons_fn(list![ElementaryFunction::CONS], NIL.into()), None);
}

#[test]
fn test_eq_fn() {
    use crate::types::{F, NIL, T};
    assert_eq!(
        eq_fn(list![ElementaryFunction::EQ, "A", "A"], NIL.into()),
        Some(T.into())
    );
    assert_eq!(
        eq_fn(list![ElementaryFunction::EQ, "A", "B"], NIL.into()),
        Some(F.into())
    );
    assert_eq!(eq_fn(list![ElementaryFunction::EQ, "A"], NIL.into()), None);
    assert_eq!(eq_fn(list![ElementaryFunction::EQ], NIL.into()), None);
}
