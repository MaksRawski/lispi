use crate::list;

use crate::interpreter::eval;
use crate::{
    elementary_functions::*,
    list_macros::compose_car_cdr,
    recursive_functions::equal,
    types::{List, NullableList, SExpression, NIL},
};

pub(crate) fn atom_fn(e_list: List, a: NullableList) -> Option<SExpression> {
    match compose_car_cdr("cadr", e_list) {
        Some(arg) => Some(atom(eval(arg, a)?).into()),
        None => Some(NIL.into()), // TODO: is this ok?
    }
}

pub(crate) fn car_fn(e_list: List, a: NullableList) -> Option<SExpression> {
    match eval(
        compose_car_cdr("cadr", e_list.clone()).or_else(|| {
            log::error!("CAR requires an argument to be a list: {}", e_list);
            None
        })?,
        a,
    )? {
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
        compose_car_cdr("cadr", e_list.clone()).or_else(|| {
            log::error!("CDR requires an argument to be a list: {}", e_list);
            None
        })?,
        a,
    )? {
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
                eval(car(arguments.clone()), a.clone())?,
                eval(
                    compose_car_cdr("cadr", arguments).or_else(|| {
                        log::error!("Second argument of cons can't be NIL: {}", e_list);
                        None
                    })?,
                    a
                )?
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
        SExpression::List(arguments) => Some(
            equal(
                eval(car(arguments.clone()), a.clone())?,
                eval(
                    compose_car_cdr("cadr", arguments).or_else(|| {
                        log::error!("EQ requires two arguments: {}", e_list);
                        None
                    })?,
                    a,
                )?,
            )
            .into(),
        ),
        SExpression::Atom(argument) => {
            panic!(
                "Invalid use of EQ: {} two arguments are required.",
                argument
            );
        }
    }
}

pub type EvalFn = fn(List, NullableList) -> Option<SExpression>;
pub static BUILTIN_FUNCS: phf::Map<&'static str, EvalFn> = phf::phf_map! {
    "ATOM" => atom_fn,
    "CAR" => car_fn,
    "CDR" => cdr_fn,
    "CONS" => cons_fn,
    "EQ" => eq_fn
};
