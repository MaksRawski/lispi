use crate::{
    elementary_functions::{atom, car, cdr, cons},
    list,
    list_functions::{append, assoc_v, pair},
    list_macros::compose_car_cdr,
    recursive_functions::{equal, null},
    types::{Atom, ElementaryFunction, List, NullableList, SExpression, Symbol, NIL, T},
};

/// The universal lisp function AKA the interpreter:
/// applies a function f to a list of arguments x
pub fn apply(f: SExpression, x: List) -> Option<SExpression> {
    // TODO: look at appendix B of programmer's manual for how to allow usage
    // of other "compiled" functions?
    eval(cons(f, appq(x.into())).into(), NIL.into())
}

/// applies QUOTE to each symbol in an expression
pub(crate) fn appq(m: SExpression) -> SExpression {
    if null(m.clone()) {
        return NIL.into();
    }
    match m {
        SExpression::List(l) => cons(appq(car(l.clone())), appq(cdr(l))).into(),
        SExpression::Atom(Atom::Symbol(_)) => m,
        SExpression::Atom(a) => cons(Symbol::QUOTE, a).into(),
    }
}

/// handles lambda S-expressions
fn handle_lambda(e: List, a: NullableList) -> Option<SExpression> {
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
fn handle_label(e: List, a: NullableList) -> Option<SExpression> {
    // the entire e is of the form: ((LABEL, function_name, expression), args)
    let function_name: String = match compose_car_cdr("cadar", e.clone()) {
        Some(SExpression::Atom(Atom::Symbol(Symbol::Other(s))))
        | Some(SExpression::Atom(Atom::String(s))) => s,
        _ => {
            log::error!("Second argument of LABEL should be a string: {}", e);
            return None;
        }
    };

    let expression = match compose_car_cdr("caddar", e.clone()) {
        Some(SExpression::Atom(_)) => todo!("Do we allow expressions to be atoms?"),
        Some(expr) => expr,
        None => {
            log::error!("LABEL is missing an associated expression: {}", e);
            return None;
        }
    };
    let args = cdr(e.clone());

    let association_list = cons(function_name, car(e));

    let new_association_list = match a {
        NullableList::List(a_list) => cons(association_list, a_list),
        NullableList::NIL => association_list,
    };

    eval(cons(expression, args).into(), new_association_list.into())
}

/// evaluates an expression using association list `a`
pub(crate) fn eval(e: SExpression, a: NullableList) -> Option<SExpression> {
    match e {
        SExpression::Atom(e_atom) => match a {
            NullableList::List(a_list) => Some(assoc_v(e_atom.clone(), a_list).unwrap_or(e_atom.into())),
            NullableList::NIL => Some(e_atom.into()),
        },
        SExpression::List(e_list) => match car(e_list.clone()) {
            SExpression::Atom(car_e) => match car_e {
                Atom::Symbol(Symbol::QUOTE) => Some(compose_car_cdr("cadr", e_list).unwrap()),
                Atom::Symbol(Symbol::COND) => match cdr(e_list.clone()){
                    SExpression::List(terms) => evcon(terms, a),
                    SExpression::Atom(_) => panic!("COND requires arguments to be of the form: (predicate expression), got: {}", e_list),
                }
                Atom::Symbol(Symbol::ElementaryFunction(f)) => match f {
                    ElementaryFunction::ATOM => match compose_car_cdr("cadr", e_list) {
                        Some(arg) => Some(atom(eval(arg, a)?).into()),
                        None => Some(NIL.into()), // TODO: is this ok?
                    },
                    ElementaryFunction::EQ => match cdr(e_list.clone()) {
                        SExpression::List(arguments) => Some(equal(
                            eval(car(arguments.clone()), a.clone())?,
                            eval(
                                compose_car_cdr("cadr", arguments).or_else(|| {
                                    log::error!("EQ requires two arguments: {}", e_list);
                                    None
                                })?,
                                a,
                            )?,
                        )
                        .into()),
                        SExpression::Atom(argument) => {
                            panic!(
                                "Invalid use of EQ: {} two arguments are required.",
                                argument
                            );
                        }
                    },
                    ElementaryFunction::CAR => match eval(
                        compose_car_cdr("cadr", e_list.clone()).or_else(|| {
                            log::error!("CAR requires an argument to be a list: {}", e_list);
                            None
                        })?,
                        a,
                    )? {
                        SExpression::List(list) => Some(car(list)),
                        SExpression::Atom(argument) => {
                            log::error!("CAR requires an argument to be a list, but was: {}", argument);
                            None
                        },
                    },
                    ElementaryFunction::CDR => match eval(
                        compose_car_cdr("cadr", e_list.clone()).or_else(|| {
                            log::error!("CDR requires an argument to be a list: {}", e_list);
                            None
                        })?,
                        a,
                    )? {
                        SExpression::List(list) => Some(cdr(list)),
                        SExpression::Atom(argument) => {
                            log::error!("CDR requires an argument to be a list, but was: {}", argument);
                            None
                        },
                    },
                    ElementaryFunction::CONS => match cdr(e_list.clone()) {
                        SExpression::List(arguments) => Some(list![
                            eval(car(arguments.clone()), a.clone())?,
                            eval(
                                compose_car_cdr("cadr", arguments).or_else(|| {
                                    log::error!("Second argument of cons can't be NIL: {}", e_list);
                                    None
                                })?,
                                a
                            )?
                        ]
                        .into()),
                        SExpression::Atom(_argument) => {
                            log::error!(
                                "Invalid use of CONS: {}, two arguments are required.",
                                e_list
                            );
                            None
                        }
                    },
                },
                Atom::Symbol(Symbol::LAMBDA) => Some(e_list.into()),
                Atom::Symbol(Symbol::LABEL) => {log::error!("Invalid use of LABEL: {}", e_list); None},
                Atom::Number(n) => {log::error!("Tried to use {n} like a function: {}", e_list); None},
                Atom::Bool(b) => {log::error!("Tried to use {b:?} like a function: {}", e_list); None},
                Atom::Symbol(Symbol::Other(s)) | Atom::String(s) => match a.clone() {
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
                    NullableList::NIL => {log::error!("Invalid function: {}, symbol unbound.", s); None},
                },
            },
            SExpression::List(list_func) => match car(list_func.clone()) {
                SExpression::Atom(Atom::Symbol(Symbol::LABEL)) => handle_label(e_list, a),
                SExpression::Atom(Atom::Symbol(Symbol::LAMBDA)) => handle_lambda(e_list, a),
                SExpression::Atom(Atom::Symbol(Symbol::QUOTE)) => Some(cdr(list_func)),
                SExpression::Atom(Atom::Number(_)) => {
                    log::error!("Tried to use a number as a function: {}", e_list);
                    None
                }
                SExpression::Atom(f) => {
                    if cdr(e_list.clone()) == NIL.into() {
                        eval(list_func.into(), a)
                    } else {
                        log::error!("Tried to use {f} like LABEL or LAMBDA: {}", e_list);
                        None
                    }
                }
                SExpression::List(_) => {log::error!("Tried to use a list as a function: {}", e_list); None},
            },
        },
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
fn test_appq() {
    use crate::list_macros::list;
    assert_eq!(
        appq(Symbol::Other("A".to_string()).into()),
        Symbol::Other("A".to_string()).into()
    );
    assert_eq!(
        appq(Atom::String("A".to_string()).into()),
        cons(Symbol::QUOTE, Atom::String("A".to_string())).into()
    );
    assert_eq!(appq(NIL.into()), NIL.into());
    assert_eq!(
        appq(ElementaryFunction::ATOM.into()),
        ElementaryFunction::ATOM.into()
    );
    assert_eq!(
        appq(
            cons(
                Symbol::Other("A".to_string()),
                Symbol::Other("B".to_string())
            )
            .into()
        ),
        cons(
            Symbol::Other("A".to_string()),
            Symbol::Other("B".to_string())
        )
        .into()
    );
    assert_eq!(
        appq(
            list![
                Atom::String("A".to_string()),
                Symbol::Other("B".to_string()),
                Atom::String("C".to_string())
            ]
            .into()
        ),
        list![
            cons(Symbol::QUOTE, Atom::String("A".to_string())),
            Symbol::Other("B".to_string()),
            cons(Symbol::QUOTE, Atom::String("C".to_string()))
        ]
        .into()
    );
    assert_eq!(
        appq(
            list![
                Symbol::Other("A".to_string()),
                list![
                    Symbol::Other("B".to_string()),
                    Atom::String("C".to_string())
                ]
            ]
            .into()
        ),
        list![
            Symbol::Other("A".to_string()),
            list![
                Symbol::Other("B".to_string()),
                cons(Symbol::QUOTE, Atom::String("C".to_string()))
            ]
        ]
        .into()
    );
}

#[cfg(test)]
mod elementary_functions_tests {
    use crate::list_macros::list;
    use crate::types::T;

    use super::*;

    #[test]
    fn test_eval_car() {
        // (car '(A B)) => A
        assert_eq!(
            eval(
                list![
                    ElementaryFunction::CAR,
                    list![Symbol::QUOTE, list!["A", "B"]]
                ]
                .into(),
                NIL.into()
            ),
            Some("A".into())
        )
    }
    #[test]
    fn test_eval_cdr() {
        // (cdr '(A B)) => '(B)
        assert_eq!(
            eval(
                list![
                    ElementaryFunction::CDR,
                    list![Symbol::QUOTE, list!["A", "B"]]
                ]
                .into(),
                NIL.into()
            ),
            Some(list!["B"].into())
        )
    }
    #[test]
    fn test_eval_cons() {
        assert_eq!(
            eval(
                list![
                    ElementaryFunction::CONS,
                    list![Symbol::QUOTE, "A"],
                    list![Symbol::QUOTE, "B"]
                ]
                .into(),
                NIL.into()
            ),
            Some(list!["A", "B"].into())
        );
        // (cons (cons 'A 'B) 'C) => (('A 'B) 'C)
        assert_eq!(
            eval(
                list![
                    ElementaryFunction::CONS,
                    list![Symbol::QUOTE, list!["A", "B"]],
                    list![Symbol::QUOTE, "C"]
                ]
                .into(),
                NIL.into()
            ),
            Some(list![list!["A", "B"], "C"].into())
        );
    }
    #[test]
    fn test_eval_eq() {
        assert_eq!(
            eval(
                list![
                    ElementaryFunction::EQ,
                    list![Symbol::QUOTE, "x"],
                    list![Symbol::QUOTE, "x"]
                ]
                .into(),
                NIL.into()
            ),
            Some(T.into())
        );
        assert_eq!(
            eval(
                list![
                    ElementaryFunction::EQ,
                    list![Symbol::QUOTE, "x"],
                    list![Symbol::QUOTE, "y"]
                ]
                .into(),
                NIL.into()
            ),
            Some(NIL.into())
        );
        assert_eq!(
            eval(
                list![ElementaryFunction::EQ, "x", "y"].into(),
                list![cons("x", 1), cons("y", 1)].into()
            ),
            Some(T.into())
        );
    }
    #[test]
    fn test_eval_atom() {
        assert_eq!(
            eval(list![ElementaryFunction::ATOM, "x"].into(), NIL.into()),
            Some(T.into())
        );
        assert_eq!(
            eval(
                list![
                    ElementaryFunction::ATOM,
                    list![Symbol::QUOTE, list![1, 2, 3]]
                ]
                .into(),
                NIL.into()
            ),
            Some(NIL.into())
        );
        assert_eq!(
            eval(
                list![ElementaryFunction::ATOM, "x"].into(),
                list![cons("x", list![1, 2, 3])].into()
            ),
            Some(NIL.into())
        );
    }
    #[test]
    fn test_eval_car_of_cons() {
        assert_eq!(
            eval(
                list![
                    ElementaryFunction::CAR,
                    list![ElementaryFunction::CONS, 1, 2]
                ]
                .into(),
                NIL.into()
            ),
            Some(1.into())
        );
        assert_eq!(
            eval(
                list![
                    ElementaryFunction::CAR,
                    list![
                        ElementaryFunction::CONS,
                        list![Symbol::QUOTE, "A"],
                        list![Symbol::QUOTE, "B"]
                    ]
                ]
                .into(),
                NIL.into()
            ),
            Some("A".into())
        );
        assert_eq!(
            eval(
                list![
                    ElementaryFunction::CAR,
                    list![
                        ElementaryFunction::CONS,
                        list![
                            ElementaryFunction::CONS,
                            list![Symbol::QUOTE, "A"],
                            list![Symbol::QUOTE, "B"]
                        ],
                        list![Symbol::QUOTE, "C"]
                    ]
                ]
                .into(),
                NIL.into()
            ),
            Some(list!["A", "B"].into())
        );
    }
}

#[cfg(test)]
mod handle_lambda_tests {
    // LAMBDA expression is of the form: ((LAMBDA, binds, expression), arg0, arg1, arg2, ...)
    use super::*;
    use crate::list_macros::list;

    #[test]
    fn test_id_lambda() {
        let lambda_expr = list![Symbol::LAMBDA, list!["x"], "x"];
        assert_eq!(
            handle_lambda(list![lambda_expr, list![Symbol::QUOTE, "A"]], NIL.into()),
            Some("A".into())
        );
    }

    #[test]
    fn test_binds_lambda() {
        let lambda = list![Symbol::LAMBDA, list!["x", "y"], "x"];

        assert_eq!(
            handle_lambda(
                list![lambda, list![Symbol::QUOTE, "A"], list![Symbol::QUOTE, "B"]],
                NIL.into()
            ),
            Some("A".into())
        );

        let lambda = list![Symbol::LAMBDA, list!["x", "y"], "y"];
        assert_eq!(
            handle_lambda(
                list![lambda, list![Symbol::QUOTE, "A"], list![Symbol::QUOTE, "B"]],
                NIL.into()
            ),
            Some("B".into())
        );
    }
    #[test]
    fn test_cond_lambda() {
        let lambda = list![
            Symbol::LAMBDA,
            list!["x", "y"],
            list![
                Symbol::COND,
                list![list![ElementaryFunction::EQ, "x", "y"], "x"],
                list![list![ElementaryFunction::ATOM, "x"], "y"],
                list![list![Symbol::QUOTE, T], list![ElementaryFunction::CAR, "x"]]
            ]
        ];

        assert_eq!(
            handle_lambda(
                list![
                    lambda.clone(),
                    list![Symbol::QUOTE, "A"],
                    list![Symbol::QUOTE, "B"]
                ],
                NIL.into()
            ),
            Some("B".into())
        );

        assert_eq!(
            handle_lambda(
                list![
                    lambda,
                    list![Symbol::QUOTE, list!["A", "B"]],
                    list![Symbol::QUOTE, "C"]
                ],
                NIL.into()
            ),
            Some("A".into())
        );
    }
}

#[cfg(test)]
mod handle_label_tests {
    // LABEL expressions are of the form: ((LABEL, function_name, expression), arg0, arg1, arg2, ...)
    use super::*;
    use crate::list_macros::list;
    #[test]
    fn test_ff() {
        // (label ff
        //   (lambda (x)
        //     (cond
        //       ((atom x) x)
        //       ('T (FF (car x))))))
        let ff = list![
            Symbol::LABEL,
            "ff",
            list![
                Symbol::LAMBDA,
                list!["x"],
                list![
                    Symbol::COND,
                    list![list![ElementaryFunction::ATOM, "x"], "x"],
                    list![
                        list![Symbol::QUOTE, T],
                        list!["ff", list![ElementaryFunction::CAR, "x"]]
                    ]
                ]
            ]
        ];

        assert_eq!(
            handle_label(list![ff.clone(), list![Symbol::QUOTE, "A"]], NIL.into()),
            Some("A".into())
        );

        assert_eq!(
            handle_label(
                list![ff.clone(), list![Symbol::QUOTE, list!["A", "B"]]],
                NIL.into()
            ),
            Some("A".into())
        );

        assert_eq!(
            handle_label(
                list![ff, list![Symbol::QUOTE, list![list!["A", "B"], "C"]]],
                NIL.into()
            ),
            Some("A".into())
        );
    }
}
