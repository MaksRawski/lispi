use crate::{
    elementary_functions::{car, cdr, cons},
    list,
    list_functions::{append, assoc_v, pair},
    list_macros::compose_car_cdr,
    types::*,
};

use super::eval;

pub(crate) fn handle_quote(e_list: List, _a: &NullableList) -> Option<SExpression> {
    compose_car_cdr("cadr", &e_list).or_else(|| {
        log::error!("QUOTE requires an argument.");
        None
    })
}

pub(crate) fn handle_cond(e_list: List, a: &NullableList) -> Option<SExpression> {
    match cdr(e_list) {
        SExpression::List(terms) => evcon(terms, a),
        SExpression::Atom(_) => {
            log::error!("COND requires at least one list as an argument.");
            None
        }
    }
}

/// The arguments of `and` are evaluated in sequence, from left to right,
/// until one is found that is false, or until the end of the list is reached.
/// The value of `and` is false or true respectively.
pub(crate) fn handle_and(e_list: List, a: &NullableList) -> Option<SExpression> {
    let h = eval(car(e_list.clone()), a)?.0;

    if h == F.into() {
        Some(F.into())
    } else {
        match cdr(e_list) {
            SExpression::Atom(t) => Some((t != F.into()).into()),
            SExpression::List(l) => handle_and(l, a),
        }
    }
}

/// The arguments of `or` are evaluated in sequence, from left to right,
/// until one is found that is true, or until the end of the list is reached.
/// The value of `or` is true or false respectively.
pub(crate) fn handle_or(e_list: List, a: &NullableList) -> Option<SExpression> {
    let h = eval(car(e_list.clone()), a)?.0;

    if h == T.into() {
        Some(T.into())
    } else {
        match cdr(e_list) {
            SExpression::Atom(t) => Some((t == T.into()).into()),
            SExpression::List(l) => handle_or(l, a),
        }
    }
}

pub(crate) fn handle_list(e_list: List, a: &NullableList) -> Option<SExpression> {
    // we're going to run this function recursively so we need to check if it's
    // the first call or another one
    let args = if car(e_list.clone()) == SpecialForm::LIST.into() {
        match cdr(e_list) {
            SExpression::Atom(_) => {
                log::error!("LIST expects at least one argument");
                return None;
            }
            SExpression::List(l) => l,
        }
    } else {
        e_list
    };
    match cdr(args.clone()) {
        SExpression::Atom(_) => Some(list![eval(car(args), a).map(|(e, _)| e)?].into()),
        SExpression::List(cdr_args) => Some(
            cons(
                eval(car(args), a).map(|(e, _)| e)?,
                handle_list(cdr_args, a)?,
            )
            .into(),
        ),
    }
}

/// called whenever a symbol is not a builtin function
pub(crate) fn handle_other_symbol(
    symbol: String,
    e: List,
    a: &NullableList,
) -> Option<SExpression> {
    if let NullableList::List(a_list) = a.clone() {
        let looked_up_symbol = assoc_v(symbol.clone().into(), a_list.clone()).or_else(|| {
            log::error!("Invalid function: {}, symbol unbound.", symbol);
            None
        })?;

        match cdr(e) {
            SExpression::List(arguments) => {
                Into::<SExpression>::into(cons(looked_up_symbol, arguments)).eval(a)
            }
            SExpression::Atom(_) => looked_up_symbol.eval(&a_list.into()),
        }
    } else {
        log::error!("Invalid function: {}, symbol unbound.", symbol);
        None
    }
}

/// evaulates the propositional terms in order, and chooses
/// the form following the first true predicate
fn evcon(c: List, a: &NullableList) -> Option<SExpression> {
    let predicate = compose_car_cdr("caar", &c).or_else(|| {
        log::error!(
            "COND doesn't have a list of predicates as its argument: {}",
            c
        );
        None
    })?;

    if eval(predicate.clone(), a)?.0 == T.into() {
        eval(
            compose_car_cdr("cadar", &c).or_else(|| {
                log::error!(
                    "No S-expression is associated with predicate: {} in: {}",
                    predicate,
                    c
                );
                None
            })?,
            a,
        )
        .map(|(e, _a)| e)
    } else {
        match cdr(c.clone()) {
            SExpression::List(l) => evcon(l, a),
            SExpression::Atom(cdr_c) => {
                if cdr_c == NIL {
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

/// Evalutes expressions in order they appeared.
/// Returns a list of results, NIL if m is NIL or None if one of the arguments
/// failed to eval.
fn evlis(m: NullableList, a: &NullableList) -> Option<NullableList> {
    match m {
        NullableList::List(m_list) => match cdr(m_list.clone()) {
            SExpression::Atom(cdr_m_list) => {
                Some(cons(eval(car(m_list), a)?.0, eval(cdr_m_list.into(), a)?.0).into())
            }
            SExpression::List(cdr_m_list) => {
                Some(cons(eval(car(m_list), a)?.0, evlis(cdr_m_list.into(), a)?).into())
            }
        },
        NullableList::NIL => Some(NIL.into()),
    }
}

pub(crate) fn handle_label(e: List, a: &NullableList) -> Option<SExpression> {
    // the entire e is of the form: ((LABEL, name, definition), args)
    let function_name: String = match compose_car_cdr("cadar", &e) {
        Some(SExpression::Atom(Atom::Symbol(Symbol::Other(s)))) => s,
        _ => match compose_car_cdr("cadr", &e) {
            Some(SExpression::Atom(Atom::Symbol(Symbol::Other(s)))) => s,
            _ => {
                log::error!("Second argument of LABEL should be a name: {}", e);
                return None;
            }
        },
    };

    let expression = match compose_car_cdr("caddar", &e) {
        Some(expr) => expr,
        None => match compose_car_cdr("caddr", &e) {
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
        NullableList::List(a_list) => cons(association_list, a_list.clone()),
        NullableList::NIL => association_list,
    };

    eval(cons(expression, args).into(), &new_association_list.into()).map(|(e, _a)| e)
}

pub(crate) fn handle_lambda(e: List, a: &NullableList) -> Option<SExpression> {
    // e is of the form: ((LAMBDA, binds, expression), args)
    let binds: List = match compose_car_cdr("cadar", &e) {
        Some(SExpression::List(l)) => l,
        _ => {
            log::error!(
                "Invalid use of LAMBDA: {}, second element should be a list of symbols to bind.",
                e
            );
            return None;
        }
    };

    let expression = compose_car_cdr("caddar", &e).or_else(|| {
        log::error!(
            "Invalid use of LAMBDA: {}, third element should be an S-expression.",
            e
        );
        None
    })?;

    // evaluate the args before putting them into the expression
    let args: List = match cdr(e.clone()) {
        SExpression::Atom(arg) => cons(eval(arg.into(), a).map(|(e, _a)| e)?, NIL),
        SExpression::List(args) => match evlis(args.into(), a)? {
            NullableList::List(evaluated_args) => evaluated_args,
            NullableList::NIL => {
                log::error!("Invalid use of LAMBDA: {}, no arguments were provided.", e);
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
        NullableList::List(a_list) => match append(bound_symbols.into(), a_list.clone().into()) {
            SExpression::List(new_alist) => new_alist,
            SExpression::Atom(_) => unreachable!(),
        },
        NullableList::NIL => bound_symbols,
    };

    eval(expression, &new_association_list.into()).map(|(e, _a)| e)
}

#[test]
fn test_evlis() {
    use crate::list_macros::list;

    assert_eq!(
        evlis(list![list![SpecialForm::QUOTE, "A"]].into(), &NIL.into()),
        Some(list!["A"].into())
    );
    assert_eq!(
        evlis(
            list![
                list![SpecialForm::QUOTE, "A"],
                list![SpecialForm::QUOTE, "B"],
                list![SpecialForm::QUOTE, "C"]
            ]
            .into(),
            &NIL.into()
        ),
        Some(list!["A", "B", "C"].into())
    );
}

#[test]
fn test_handle_quote() {
    use crate::list_macros::list;

    assert_eq!(
        handle_quote(list![SpecialForm::QUOTE, list!["A", "B", "C"]], &NIL.into()),
        Some(list!["A", "B", "C"].into())
    );
    assert_eq!(
        handle_quote(
            list![SpecialForm::QUOTE, list![list!["A"], list!["B", "C"]]],
            &NIL.into()
        ),
        Some(list![list!["A"], list!["B", "C"]].into())
    );
    assert_eq!(
        handle_quote(list![SpecialForm::QUOTE, NIL], &NIL.into()),
        Some(NIL.into())
    );
    assert_eq!(handle_quote(list![SpecialForm::QUOTE], &NIL.into()), None);
}

#[test]
fn test_and() {
    use crate::list_macros::list;

    assert_eq!(handle_and(list![T, T, T], &NIL.into()), Some(true.into()));
    assert_eq!(handle_and(list![T, T, F], &NIL.into()), Some(false.into()));
    assert_eq!(handle_and(list![1, 2, 3], &NIL.into()), Some(true.into()));
}

#[test]
fn test_or() {
    use crate::list_macros::list;

    assert_eq!(handle_or(list![F, F, T], &NIL.into()), Some(true.into()));
    assert_eq!(handle_or(list![F, F, F], &NIL.into()), Some(false.into()));
    assert_eq!(handle_or(list![1, 2, 3], &NIL.into()), Some(false.into()));
}

#[test]
fn test_handle_other_symbol() {
    use crate::list_macros::list;

    let a: NullableList = list![cons("test", 123)].into();
    assert_eq!(
        handle_other_symbol("test".into(), list!["test"], &a),
        Some(123.into())
    );

    let a: NullableList = list![cons("first", ElementaryFunction::CAR)].into();
    assert_eq!(
        handle_other_symbol(
            "first".into(),
            list!["first", list![ElementaryFunction::CONS, 1, 2]],
            &a
        ),
        Some(1.into())
    );

    let a: NullableList = list![cons("first", ElementaryFunction::CAR)].into();
    assert_eq!(
        handle_other_symbol(
            "first".into(),
            list![
                "first",
                list![
                    ElementaryFunction::CONS,
                    list![ElementaryFunction::CONS, 1, 2],
                    3
                ]
            ],
            &a
        ),
        Some((cons(1, 2)).into())
    );

    let a: NullableList = list![cons("mycons", ElementaryFunction::CONS)].into();
    assert_eq!(
        handle_other_symbol("mycons".into(), list!["mycons", 1, 2], &a),
        Some(cons(1, 2).into())
    );

    assert_eq!(
        handle_other_symbol("test".into(), list!["test"], &NIL.into()),
        None
    );
    assert_eq!(
        handle_other_symbol("first".into(), list!["first"], &list!["test", 123].into()),
        None
    );
}

#[test]
fn test_handle_label() {
    use crate::list_macros::list;
    // (label ff
    //   (lambda (x)
    //     (cond
    //       ((atom x) x)
    //       ('T (FF (car x))))))
    let ff = list![
        SpecialForm::LABEL,
        "ff",
        list![
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
        ]
    ];

    assert_eq!(
        handle_label(
            list![ff.clone(), list![SpecialForm::QUOTE, "A"]],
            &NIL.into()
        ),
        Some("A".into())
    );

    assert_eq!(
        handle_label(
            list![ff.clone(), list![SpecialForm::QUOTE, list!["A", "B"]]],
            &NIL.into()
        ),
        Some("A".into())
    );

    assert_eq!(
        handle_label(
            list![ff, list![SpecialForm::QUOTE, list![list!["A", "B"], "C"]]],
            &NIL.into()
        ),
        Some("A".into())
    );
}

#[cfg(test)]
mod handle_lambda_tests {
    // LAMBDA expression is of the form: ((LAMBDA, binds, expression), arg0, arg1, arg2, ...)
    use super::*;
    use crate::list_macros::list;

    #[test]
    fn test_id_lambda() {
        let lambda_expr = list![SpecialForm::LAMBDA, list!["x"], "x"];
        assert_eq!(
            handle_lambda(
                list![lambda_expr, list![SpecialForm::QUOTE, "A"]],
                &NIL.into()
            ),
            Some("A".into())
        );
    }

    #[test]
    fn test_binds_lambda() {
        let lambda = list![SpecialForm::LAMBDA, list!["x", "y"], "x"];

        assert_eq!(
            handle_lambda(
                list![
                    lambda,
                    list![SpecialForm::QUOTE, "A"],
                    list![SpecialForm::QUOTE, "B"]
                ],
                &NIL.into()
            ),
            Some("A".into())
        );

        let lambda = list![SpecialForm::LAMBDA, list!["x", "y"], "y"];
        assert_eq!(
            handle_lambda(
                list![
                    lambda,
                    list![SpecialForm::QUOTE, "A"],
                    list![SpecialForm::QUOTE, "B"]
                ],
                &NIL.into()
            ),
            Some("B".into())
        );
    }
    #[test]
    fn test_cond_lambda() {
        let lambda = list![
            SpecialForm::LAMBDA,
            list!["x", "y"],
            list![
                SpecialForm::COND,
                list![list![BuiltinFunc::EQUAL, "x", "y"], "x"],
                list![list![ElementaryFunction::ATOM, "x"], "y"],
                list![
                    list![SpecialForm::QUOTE, T],
                    list![ElementaryFunction::CAR, "x"]
                ]
            ]
        ];

        assert_eq!(
            handle_lambda(
                list![
                    lambda.clone(),
                    list![SpecialForm::QUOTE, "A"],
                    list![SpecialForm::QUOTE, "B"]
                ],
                &NIL.into()
            ),
            Some("B".into())
        );

        assert_eq!(
            handle_lambda(
                list![
                    lambda,
                    list![SpecialForm::QUOTE, list!["A", "B"]],
                    list![SpecialForm::QUOTE, "C"]
                ],
                &NIL.into()
            ),
            Some("A".into())
        );
    }
}
