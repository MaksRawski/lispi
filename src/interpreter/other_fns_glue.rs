use crate::{
    elementary_functions::{car, cdr, cons},
    list,
    list_macros::compose_car_cdr,
    recursive_functions::equal,
    types::{Atom, List, NullableList, SExpression, SpecialForm},
};

use super::eval;

pub(crate) fn equal_fn(e: List, a: NullableList) -> Option<SExpression> {
    let args = if let SExpression::List(l) = cdr(e) {
        l
    } else {
        log::error!("EQUAL requires two arguments");
        return None;
    };

    let x = eval(car(args.clone()), a.clone())?.0;
    let y = eval(compose_car_cdr("cadr", args)?, a)?.0;
    Some(equal(x, y).into())
}

pub(crate) fn sum_fn(e: List, a: NullableList) -> Option<SExpression> {
    let args = if let SExpression::List(l) = cdr(e) {
        l
    } else {
        log::error!("EQUAL requires two arguments");
        return None;
    };
    let x = eval(car(args.clone()), a.clone())?.0;
    if let SExpression::Atom(Atom::Number(x)) = x {
        let y = eval(compose_car_cdr("cadr", args)?, a)?.0;
        match y {
            SExpression::Atom(Atom::Number(y)) => Some((x + y).into()),
            _ => {
                log::error!("Tried to sum {x} with non-number: {y}");
                None
            }
        }
    } else {
        log::error!("Tried to sum non-number: {x}");
        None
    }
}

pub(crate) fn prdct_fn(e: List, a: NullableList) -> Option<SExpression> {
    let args = if let SExpression::List(l) = cdr(e) {
        l
    } else {
        log::error!("EQUAL requires two arguments");
        return None;
    };
    let x = eval(car(args.clone()), a.clone())?.0;
    if let SExpression::Atom(Atom::Number(x)) = x {
        let y = eval(compose_car_cdr("cadr", args)?, a)?.0;
        match y {
            SExpression::Atom(Atom::Number(y)) => Some((x * y).into()),
            _ => {
                log::error!("Tried to take a prdct of {x} with non-number: {y}");
                None
            }
        }
    } else {
        log::error!("Tried to take a prdct of non-number: {x}");
        None
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
