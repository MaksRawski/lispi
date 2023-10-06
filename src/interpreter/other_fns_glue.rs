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
