use crate::{
    elementary_functions::{car, cdr, cons},
    list_functions::{append, assoc_v, pair},
    list_macros::compose_car_cdr,
    types::*,
};

use super::eval;

/// handles lambda S-expressions
pub(crate) fn handle_lambda(e: List, a: NullableList) -> Option<SExpression> {
    // e is of the form: ((LAMBDA, binds, expression), args)
    let binds: List = match compose_car_cdr("cadar", e.clone()) {
        Some(SExpression::List(l)) => l,
        _ => {
            log::error!(
                "Invalid use of LAMBDA: {}, second element should be a list of symbols to bind.",
                e
            );
            return None;
        }
    };

    let expression = compose_car_cdr("caddar", e.clone()).or_else(|| {
        log::error!(
            "Invalid use of LAMBDA: {}, third element should be an S-expression.",
            e
        );
        None
    })?;
    // evaluate the args before putting them into the expression
    let args: List = match cdr(e.clone()) {
        SExpression::Atom(arg) => cons(eval(arg.into(), a.clone())?, NIL),
        SExpression::List(args) => match evlis(args.into(), a.clone())? {
            NullableList::List(evaluated_args) => evaluated_args,
            NullableList::NIL => {
                log::error!(
                    "Invalid use of LAMBDA: {}, Failed to eval the arguments.",
                    e
                );
                return None;
            }
        },
    };

    let bound_symbols = match pair(binds.into(), args.into()) {
        NullableList::List(l) => l,
        NullableList::NIL => {
            log::error!(
                "Invalid use of LAMBDA: {}, no arguments to evaluate the lambda were provided.",
                e
            );
            return None;
        }
    };

    let new_association_list = match a {
        NullableList::List(a_list) => match append(bound_symbols.into(), a_list.into()) {
            SExpression::List(new_alist) => new_alist,
            SExpression::Atom(_) => unreachable!(),
        },
        NullableList::NIL => bound_symbols,
    };

    eval(expression, new_association_list.into())
}

/// utility function for the monstrocity that is `eval`
pub(crate) fn handle_label(e: List, a: NullableList) -> Option<SExpression> {
    // the entire e is of the form: ((LABEL, name, definition), args)
    let function_name: String = match compose_car_cdr("cadar", e.clone()) {
        Some(SExpression::Atom(Atom::Symbol(Symbol::Other(s)))) => s,
        _ => match compose_car_cdr("cadr", e.clone()) {
            Some(SExpression::Atom(Atom::Symbol(Symbol::Other(s)))) => s,
            _ => {
                log::error!("Second argument of LABEL should be a name: {}", e);
                return None;
            }
        },
    };

    let expression = match compose_car_cdr("caddar", e.clone()) {
        Some(expr) => expr,
        None => match compose_car_cdr("caddr", e.clone()) {
            Some(expr) => expr,
            None => {
                log::error!("LABEL is missing a definition: {}", e);
                return None;
            }
        },
    };
    let args = cdr(e.clone());

    let association_list = cons(function_name, car(e));

    let new_association_list = match a {
        NullableList::List(a_list) => cons(association_list, a_list),
        NullableList::NIL => association_list,
    };

    eval(cons(expression, args).into(), new_association_list.into())
}

pub(crate) fn handle_quote(e_list: List, _a: NullableList) -> Option<SExpression> {
    compose_car_cdr("cadr", e_list)
}

pub(crate) fn handle_cond(e_list: List, a: NullableList) -> Option<SExpression> {
    match cdr(e_list.clone()) {
        SExpression::List(terms) => evcon(terms, a),
        SExpression::Atom(_) => {
            log::error!(
                "COND requires arguments to be of the form: (predicate expression), got: {}",
                e_list
            );
            None
        }
    }
}

/// The arguments of and are evaluated in sequence, from left to right,
/// until one is found that is false, or until the end of the list is reached.
/// The value of and is false or true respectively.
pub(crate) fn handle_and(e_list: List, a: NullableList) -> Option<SExpression> {
    let h = eval(car(e_list.clone()), a.clone())?;

    if h == F.into() {
        Some(F.into())
    } else {
        match cdr(e_list) {
            SExpression::Atom(t) => Some((t != F.into()).into()),
            SExpression::List(l) => handle_and(l, a),
        }
    }
}

/// The arguments of and are evaluated in sequence, from left to right,
/// until one is found that is false, or until the end of the list is reached.
/// The value of and is false or true respectively.
pub(crate) fn handle_or(e_list: List, a: NullableList) -> Option<SExpression> {
    let h = eval(car(e_list.clone()), a.clone())?;

    if h == T.into() {
        Some(T.into())
    } else {
        match cdr(e_list) {
            SExpression::Atom(t) => Some((t == T.into()).into()),
            SExpression::List(l) => handle_or(l, a),
        }
    }
}

/// this function is called basically whenever the symbol is not an elementary function
/// TODO: check if the symbol is defined using SUBR or EXPR, otherwise check `a`
/// (which is the only thing that it's doing now)
pub(crate) fn handle_other_symbol(s: String, e_list: List, a: NullableList) -> Option<SExpression> {
    match a.clone() {
        NullableList::List(a_list) => match cdr(e_list) {
            SExpression::Atom(argument) => eval(
                cons(
                    assoc_v(s.clone().into(), a_list).or_else(|| {
                        log::error!("Invalid function: {}, symbol unbound.", s);
                        None
                    })?,
                    eval(argument.into(), a.clone())?,
                )
                .into(),
                a,
            ),
            SExpression::List(arguments) => eval(
                cons(
                    assoc_v(s.clone().into(), a_list).or_else(|| {
                        log::error!("Invalid function: {}, symbol unbound.", s);
                        None
                    })?,
                    eval(arguments.into(), a.clone())?, // NOTE: paper claims that evlis should be used here
                )
                .into(),
                a,
            ),
        },
        NullableList::NIL => {
            log::error!("Invalid function: {}, symbol unbound.", s);
            None
        }
    }
}

/// evaulates the propositional terms in order, and chooses
/// the form following the first true predicate
fn evcon(c: List, a: NullableList) -> Option<SExpression> {
    let predicate = compose_car_cdr("caar", c.clone()).or_else(|| {
        log::error!(
            "COND doesn't have a list of predicates as its argument: {}",
            c
        );
        None
    })?;

    if eval(predicate.clone(), a.clone())? == T.into() {
        eval(
            compose_car_cdr("cadar", c.clone()).or_else(|| {
                log::error!(
                    "No S-expression is associated with predicate: {} in: {}",
                    predicate,
                    c
                );
                None
            })?,
            a,
        )
    } else {
        match cdr(c.clone()) {
            SExpression::List(l) => evcon(l, a),
            SExpression::Atom(cdr_c) => {
                if cdr_c == NIL.into() {
                    log::info!("All clauses failed in: {}", c);
                    Some(NIL.into())
                } else {
                    log::error!("Unexpected atom {} found in COND: {}", cdr_c, c);
                    None
                }
            }
        }
    }
}

/// evalutes expressions in order they appeared
/// returns a list of results or NIL if m is NIL
fn evlis(m: NullableList, a: NullableList) -> Option<NullableList> {
    match m {
        NullableList::List(m_list) => match cdr(m_list.clone()) {
            SExpression::Atom(cdr_m_list) => {
                Some(cons(eval(car(m_list), a.clone())?, eval(cdr_m_list.into(), a)?).into())
            }
            SExpression::List(cdr_m_list) => {
                Some(cons(eval(car(m_list), a.clone())?, evlis(cdr_m_list.into(), a)?).into())
            }
        },
        NullableList::NIL => Some(NIL.into()),
    }
}

#[test]
fn test_and() {
    use crate::list;
    assert_eq!(handle_and(list![T, T, T], NIL.into()), Some(true.into()));
    assert_eq!(handle_and(list![T, T, F], NIL.into()), Some(false.into()));
    assert_eq!(handle_and(list![1, 2, 3], NIL.into()), Some(true.into()));
}
