use crate::{
    elementary_functions::{car, cdr, cons},
    list,
    list_macros::compose_car_cdr,
    recursive_functions::equal,
    types::{Atom, List, NullableList, SExpression, SpecialForm},
};

use super::eval;

pub(crate) fn equal_fn(e: List, a: NullableList) -> Option<SExpression> {
    match cdr(e.clone()) {
        SExpression::List(arguments) => {
            let x = eval(car(arguments.clone()), a.clone())?.0;
            let y = eval(
                compose_car_cdr("cadr", arguments).or_else(|| {
                    log::error!(
                        "EQUAL requires two arguments, but only one was provided: {}",
                        e
                    );
                    None
                })?,
                a,
            )?
            .0;
            Some(equal(x, y).into())
        }
        SExpression::Atom(_) => {
            log::error!("Invalid use of EQUAL: {} two arguments are required.", e);
            None
        }
    }
}

pub(crate) fn sum_fn(e: List, a: NullableList) -> Option<SExpression> {
    match cdr(e.clone()) {
        SExpression::List(arguments) => {
            let x = eval(car(arguments.clone()), a.clone())?.0;
            let y = eval(
                compose_car_cdr("cadr", arguments).or_else(|| {
                    log::error!(
                        "SUM requires two arguments, but only one was provided: {}",
                        e
                    );
                    None
                })?,
                a,
            )?
            .0;
            if let SExpression::Atom(Atom::Number(nx)) = x {
                if let SExpression::Atom(Atom::Number(ny)) = y {
                    Some((nx + ny).into())
                } else {
                    log::error!("Tried to sum non-number {y} in: {e}");
                    None
                }
            } else {
                log::error!("Tried to sum non-number {x} in: {e}");
                None
            }
        }
        SExpression::Atom(_) => {
            log::error!("Invalid use of SUM: {} two arguments are required.", e);
            None
        }
    }
}

pub(crate) fn prdct_fn(e: List, a: NullableList) -> Option<SExpression> {
    match cdr(e.clone()) {
        SExpression::List(arguments) => {
            let x = eval(car(arguments.clone()), a.clone())?.0;
            let y = eval(
                compose_car_cdr("cadr", arguments).or_else(|| {
                    log::error!(
                        "SUM requires two arguments, but only one was provided: {}",
                        e
                    );
                    None
                })?,
                a,
            )?
            .0;
            if let SExpression::Atom(Atom::Number(nx)) = x {
                if let SExpression::Atom(Atom::Number(ny)) = y {
                    Some((nx * ny).into())
                } else {
                    log::error!("Tried to sum non-number {y} in: {e}");
                    None
                }
            } else {
                log::error!("Tried to sum non-number {x} in: {e}");
                None
            }
        }
        SExpression::Atom(_) => {
            log::error!("Invalid use of SUM: {} two arguments are required.", e);
            None
        }
    }
}

/// define's argument is a list of pairs ((u1, v1), (u2, v2), ...)
/// where each u is a name and each v is a λ-expression or a function
///
/// always returns NIL (and a new association list)
pub(crate) fn define_fn(e: List, a: NullableList) -> Option<List> {
    // we're going to run this function recursively so we need to check if it's
    // the first call or another one
    let args = if car(e.clone()) == SpecialForm::DEFINE.into() {
        match cdr(e.clone()) {
            SExpression::Atom(_) => {
                log::error!("DEFINE expects its argument to be a list of pairs: {}", e);
                return None;
            }
            SExpression::List(l) => l,
        }
    } else {
        e.clone()
    };

    if let SExpression::List(l) = car(args.clone()) {
        let u = car(l.clone());
        let v = compose_car_cdr("cadr", l)?;
        let new_a_list = match a {
            NullableList::List(a_list) => cons(cons(u, v), a_list),
            NullableList::NIL => list![cons(u, v)],
        };
        // if there are more definitions
        if let SExpression::List(cdr_l) = cdr(args) {
            define_fn(cdr_l, new_a_list.into())
        } else {
            Some(new_a_list)
        }
    } else {
        log::error!("DEFINE expects its argument to be a list of pairs: {}", e);
        None
    }
}

#[test]
fn test_equal_fn() {
    use crate::types::{NIL, T};
    assert_eq!(
        equal_fn(
            list![
                BuiltinFunc::EQUAL,
                list![SpecialForm::QUOTE, "X"],
                list![SpecialForm::QUOTE, "X"]
            ],
            NIL.into()
        ),
        Some(T.into())
    );
    assert_eq!(equal_fn(list![BuiltinFunc::EQUAL], NIL.into()), None);
    assert_eq!(
        equal_fn(
            list![BuiltinFunc::EQUAL, list![SpecialForm::QUOTE, "X"]],
            NIL.into()
        ),
        None
    );
}

#[test]
fn test_sum_fn() {
    use crate::types::NIL;
    assert_eq!(
        sum_fn(list![BuiltinFunc::SUM, 1.5, 2.4], NIL.into()),
        Some(3.9.into())
    );
    assert_eq!(
        sum_fn(list![BuiltinFunc::SUM, 0, -42], NIL.into()),
        Some((-42).into())
    );
    assert_eq!(sum_fn(list![BuiltinFunc::SUM, 0], NIL.into()), None);
    assert_eq!(sum_fn(list![BuiltinFunc::SUM], NIL.into()), None);
}

#[test]
fn test_prdct_fn() {
    use crate::types::NIL;
    assert_eq!(
        prdct_fn(list![BuiltinFunc::PRDCT, 1.5, 2], NIL.into()),
        Some(3.into())
    );
    assert_eq!(
        prdct_fn(list![BuiltinFunc::PRDCT, -1, 0], NIL.into()),
        Some(0.into())
    );
    assert_eq!(prdct_fn(list![BuiltinFunc::PRDCT, 0], NIL.into()), None);
    assert_eq!(prdct_fn(list![BuiltinFunc::PRDCT], NIL.into()), None);
}

#[test]
fn test_define_fn() {
    use crate::types::NIL;

    assert_eq!(
        define_fn(
            list![SpecialForm::DEFINE, list!["first", ElementaryFunction::CAR]],
            NIL.into()
        ),
        Some(list![cons("first", ElementaryFunction::CAR)])
    );

    let ff = list![
        SpecialForm::LAMBDA,
        list!["x"],
        list![
            SpecialForm::COND,
            list![list![ElementaryFunction::ATOM, "x"], "x"],
            list![
                list![SpecialForm::QUOTE, T],
                list!["ff", list![ElementaryFunction::CAR, "x"]]
            ]
        ]
    ];

    assert_eq!(
        define_fn(
            list![SpecialForm::DEFINE, list!["ff", ff.clone()]],
            NIL.into()
        ),
        Some(list![cons("ff", ff.clone())])
    );

    // the last definition always ends up on top of the association list
    assert_eq!(
        define_fn(
            list![
                SpecialForm::DEFINE,
                list!["ff", ff.clone()],
                list!["first", ElementaryFunction::CAR]
            ],
            NIL.into()
        ),
        Some(list![
            cons("first", ElementaryFunction::CAR),
            cons("ff", ff)
        ])
    );
}
