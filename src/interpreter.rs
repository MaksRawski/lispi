use crate::{
    elementary_functions::{atom, car, cdr, cons},
    list_functions::{append, assoc_v, pair},
    list_macros::{compose_car_cdr, list},
    recursive_functions::equal,
    types::{Atom, Bool, ElementaryFunction, List, NullableList, SExpression, Symbol, NIL},
};

/// The universal lisp function AKA the interpreter:
/// applies function f to a list of arguments x
pub fn apply(f: SExpression, x: List) -> SExpression {
    // TODO: look at appendix B for how to allow usage of other "compiled" functions?
    eval(cons(f, appq(x.into()).into()).into(), NIL.into())
}

/// applies QUOTE to each symbol in an expression
/// this is different from what was mentioned in the original paper
/// look at
fn appq(m: SExpression) -> SExpression {
    match m {
        SExpression::List(l) => cons(appq(car(l.clone())).into(), appq(cdr(l)).into()).into(),
        SExpression::Atom(Atom::Bool(Bool::NIL)) => NIL.into(),
        SExpression::Atom(a) => cons(Symbol::QUOTE.into(), a.into()).into(),
    }
}

/// utility function for the monstrocity that is `eval`
fn handle_lambda(e: List, a: NullableList) -> SExpression {
    // e is of the form: ((LAMBDA, binds, expression) args)
    let binds: List = match compose_car_cdr("cadar", e.clone()) {
        Some(SExpression::List(l)) => l,
        _ => panic!(
            "Invalid use of LAMBDA: {}, second argument should be a list of symbols to bind.",
            e
        ),
    };

    let expression = compose_car_cdr("caddr", e.clone()).unwrap_or_else(|| {
        panic!(
            "Invalid use of LAMBDA: {}, third argument should be an S-expression.",
            e
        )
    });

    // evaluate the args before putting them into the expression
    let args: List = match cdr(e.clone()) {
        SExpression::Atom(arg) => cons(eval(arg.into(), a.clone()), NIL.into()),
        SExpression::List(args) => match evlis(args.into(), a.clone()) {
            NullableList::List(evaluated_args) => evaluated_args,
            NullableList::NIL => panic!(
                "Invalid use of LAMBDA: {}, Failed to eval the arguments.",
                e
            ),
        },
    };

    let bound_symbols = match pair(binds.into(), args.into()) {
        NullableList::List(l) => l,
        NullableList::NIL => panic!(
            "Invalid use of LAMBDA: {}, no arguments to evaluate the lambda were provided.",
            e
        ),
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
fn handle_label(e: List, a: NullableList) -> SExpression {
    // the entire e is of the form: ((LABEL, function_name, lambda), args)

    let function_name: String = match compose_car_cdr("cadar", e.clone()) {
        Some(SExpression::Atom(Atom::Symbol(Symbol::Other(s))))
        | Some(SExpression::Atom(Atom::String(s))) => s,
        _ => panic!(
            "Invalid use of LABEL: {}, second argument should be a function name (string).",
            e,
        ),
    };

    // 3rd argument should be a lambda which itself has a LAMBDA symbol as the first argument
    let lambda = match compose_car_cdr("caddr", e.clone()) {
        Some(SExpression::List(l)) => match car(l.clone()) {
            SExpression::Atom(Atom::Symbol(Symbol::LAMBDA)) => l,
            _ => panic!(
                "Invalid use of LABEL: {}, third argument should be a LAMBDA S-expression.",
                e
            ),
        },
        _ => panic!(
            "Invalid use of LABEL: {}, third argument should be a LAMBDA S-expression.",
            e
        ),
    };
    let args = cdr(e);

    // TODO: TEST THIS: paper claims you should associate function name with
    // (LABEL, function_name, lambda) itself, which kinda feels too recursive?
    let association_list = cons(
        function_name.clone().into(),
        list![Symbol::LABEL, function_name, lambda.clone()].into(),
    );

    let new_association_list = match a {
        NullableList::List(a_list) => cons(association_list.into(), a_list.into()),
        NullableList::NIL => association_list,
    };

    dbg!(eval(
        cons(lambda.into(), args).into(),
        new_association_list.into(),
    ))
}

// TODO: good lord that's a huge function, split into smaller ones!
/// evaluates an expression using association list `a`
fn eval(e: SExpression, a: NullableList) -> SExpression {
    match e {
        SExpression::Atom(e_atom) => match a {
            NullableList::List(a_list) => assoc_v(e_atom.clone(), a_list).unwrap_or(e_atom.into()),
            NullableList::NIL => e_atom.into(),
        },
        SExpression::List(e_list) => match car(e_list.clone()) {
            SExpression::Atom(car_e) => match car_e {
                Atom::Symbol(Symbol::QUOTE) => cdr(e_list),
                Atom::Symbol(Symbol::COND) => match cdr(e_list.clone()){
                    SExpression::List(cdr_e_list) => evcon(cdr_e_list, a).unwrap_or(NIL.into()),
                    SExpression::Atom(_) => panic!("Invalid use of COND: {}, argument should be an association list between predicates and S-expressions.", e_list),
                }
                Atom::Symbol(Symbol::ElementaryFunction(f)) => match f {
                    ElementaryFunction::ATOM => atom(eval(cdr(e_list), a)).into(),
                    ElementaryFunction::EQ => match cdr(e_list) {
                        SExpression::List(arguments) => {
                            equal(eval(car(arguments.clone()), a.clone()), eval(cdr(arguments), a)).into()
                        }
                        SExpression::Atom(argument) => {
                            panic!("Invalid use of EQ: {} two arguments are required.", argument);
                        }
                    },
                    ElementaryFunction::CAR => match eval(cdr(e_list.clone()), a) {
                        SExpression::List(list) => car(list),
                        SExpression::Atom(_argument) => panic!(
                            "Invalid use of CAR: {}, argument should be a list.",
                            e_list
                        ),
                    },
                    ElementaryFunction::CDR => match eval(cdr(e_list.clone()), a) {
                        SExpression::List(list) => cdr(list),
                        SExpression::Atom(_argument) => panic!(
                            "Invalid use of CDR: {}, argument should be a list.",
                            e_list
                        ),
                    },
                    ElementaryFunction::CONS => match cdr(e_list.clone()) {
                        SExpression::List(arguments) => cons(eval(car(arguments.clone()), a.clone()), dbg!(eval(dbg!(cdr(arguments)), a))).into(),
                        SExpression::Atom(_argument) => {
                            panic!("Invalid use of EQ: {}, two arguments are required.", e_list)
                        }
                    },
                },
                Atom::Symbol(Symbol::LAMBDA) => e_list.into(),
                Atom::Symbol(Symbol::LABEL) => panic!("Invalid use of LABEL: {}", e_list),
                Atom::Number(n) => panic!("Invalid function: {}", n),
                Atom::Bool(b) => panic!("Invalid function: {:?}", b),
                Atom::Symbol(Symbol::Other(s)) | Atom::String(s) => match a.clone() {
                    NullableList::List(a_list) => match cdr(e_list) {
                        SExpression::Atom(argument) => eval(
                            cons(
                                assoc_v(s.clone().into(), a_list).unwrap_or_else(|| {
                                    panic!("Invalid function: {}, symbol unbound.", s)
                                }),
                                eval(argument.into(), a.clone()),
                            )
                            .into(),
                            a,
                        ),
                        SExpression::List(arguments) => eval(
                            cons(
                                assoc_v(s.clone().into(), a_list).unwrap_or_else(|| {
                                    panic!("Invalid function: {}, symbol unbound.", s)
                                }),
                                evlis(arguments.into(), a.clone().into()).into()
                            )
                            .into(),
                            a,
                        ),
                    },
                    NullableList::NIL => panic!("Invalid function: {}, symbol unbound.", s),
                },
            },
            SExpression::List(list_func) => match car(list_func.clone()){
                SExpression::Atom(Atom::Symbol(Symbol::LABEL)) => handle_label(e_list, a),
                SExpression::Atom(Atom::Symbol(Symbol::LAMBDA)) => handle_lambda(e_list, a),
                SExpression::Atom(Atom::Symbol(Symbol::QUOTE)) => cdr(list_func),
                SExpression::Atom(Atom::Number(_)) => panic!("Tried to use a number as a function: {}", e_list),
                SExpression::Atom(_func) => panic!("Only LABEL and LAMBDA can be used this way: {}", e_list),
                SExpression::List(_) => panic!("Tried to use a list as a function: {}", e_list)
            },
        },
    }
}

/// evaulates the propositional terms in order, and chooses
/// the form following the first true predicate
fn evcon(c: List, a: NullableList) -> Option<SExpression> {
    match car(c.clone()) {
        SExpression::Atom(car_c) => match eval(car_c.into(), a.clone()) {
            SExpression::Atom(Atom::Bool(Bool::T)) => Some(cdr(c)),
            _ => None,
        },
        SExpression::List(car_c) => match eval(car(car_c.clone()), a.clone()) {
            SExpression::Atom(Atom::Bool(Bool::T)) => Some(eval(cdr(car_c), a)),
            _ => match cdr(c) {
                SExpression::List(cdr_c) => evcon(cdr_c, a),
                SExpression::Atom(_) => todo!(),
            },
        },
    }
}

/// evalutes expressions in order they appeared
/// returns a list of results or NIL if m is NIL
fn evlis(m: NullableList, a: NullableList) -> NullableList {
    match m {
        NullableList::List(m_list) => match cdr(m_list.clone()) {
            SExpression::Atom(cdr_m_list) => cons(
                eval(car(m_list), a.clone()),
                eval(cdr_m_list.into(), a).into(),
            )
            .into(),
            SExpression::List(cdr_m_list) => cons(
                eval(car(m_list), a.clone()),
                evlis(cdr_m_list.into(), a).into(),
            )
            .into(),
        },
        NullableList::NIL => NIL.into(),
    }
}

#[test]
fn test_appq() {
    assert_eq!(
        appq("A".into()),
        cons(Symbol::QUOTE.into(), "A".into()).into()
    );
    assert_eq!(
        appq(cons("A".into(), "B".into()).into()),
        list![
            cons(Symbol::QUOTE.into(), "A".into()),
            cons(Symbol::QUOTE.into(), "B".into())
        ]
        .into()
    );
    assert_eq!(
        appq(list!["A", "B", "C"].into()),
        list![
            cons(Symbol::QUOTE.into(), "A".into()),
            cons(Symbol::QUOTE.into(), "B".into()),
            cons(Symbol::QUOTE.into(), "C".into())
        ]
        .into()
    );
    assert_eq!(
        appq(list![list!["A", "B"], "C"].into()),
        list![
            list![
                cons(Symbol::QUOTE.into(), "A".into()),
                cons(Symbol::QUOTE.into(), "B".into())
            ],
            cons(Symbol::QUOTE.into(), "C".into())
        ]
        .into()
    );
}

#[cfg(test)]
mod elementary_functions_tests {
    use crate::types::T;

    use super::*;

    #[test]
    fn test_eval_car() {
        assert_eq!(
            eval(
                list![
                    ElementaryFunction::CAR,
                    list![list![
                        cons(Symbol::QUOTE.into(), "A".into()),
                        cons(Symbol::QUOTE.into(), "B".into())
                    ]]
                ]
                .into(),
                NIL.into()
            ),
            1.into()
        )
    }
    #[test]
    fn test_eval_cdr() {
        assert_eq!(
            eval(
                list![ElementaryFunction::CDR, list![list![Symbol::QUOTE, "A"], 2]].into(),
                NIL.into()
            ),
            "B".into()
        )
    }
    #[test]
    fn test_eval_cons() {
        assert_eq!(
            eval(list![ElementaryFunction::CONS, "A", "B"].into(), NIL.into()),
            cons("A".into(), "B".into()).into()
        );
    }
    #[test]
    fn test_eval_cons_variables() {
        assert_eq!(
            eval(
                list![ElementaryFunction::CONS, "A", "B"].into(),
                list![cons("A".into(), 1.into()), cons("B".into(), 2.into())].into()
            ),
            cons(1.into(), 2.into()).into()
        );
    }
    #[test]
    fn test_eval_eq_variables() {
        assert_eq!(
            eval(
                list![ElementaryFunction::EQ, list!["x", "x"]].into(),
                NIL.into()
            ),
            T.into()
        );
        assert_eq!(
            eval(
                list![ElementaryFunction::EQ, list!["x", "y"]].into(),
                NIL.into()
            ),
            NIL.into()
        );
        assert_eq!(
            eval(
                list![ElementaryFunction::EQ, list!["x", "y"]].into(),
                list![cons("y".into(), "x".into())].into()
            ),
            T.into()
        );
    }
    #[test]
    fn test_eval_atom() {
        assert_eq!(
            eval(list![ElementaryFunction::ATOM, 1].into(), NIL.into()),
            T.into()
        );
        assert_eq!(
            eval(
                list![ElementaryFunction::ATOM, "X"].into(),
                list![cons("X".into(), list![1, 2, 3].into())].into()
            ),
            NIL.into()
        );
    }
}

#[cfg(test)]
mod handle_lambda_tests {
    use super::*;

    #[test]
    fn test_id_lambda() {
        let lambda_expr = list![Symbol::LAMBDA, list!["x"], "x"];
        let args = list!["A"];
        assert_eq!(
            handle_lambda(list![lambda_expr, args.clone()], NIL.into()),
            "A".into()
        );
    }

    #[test]
    fn test_binds_lambda() {
        assert_eq!(
            handle_lambda(
                list![list![Symbol::LAMBDA, list!["x", "y"], "x"], list!["A", "B"]],
                NIL.into()
            ),
            "A".into()
        );
        assert_eq!(
            handle_lambda(
                list![list![Symbol::LAMBDA, list!["x", "y"], "y"], list!["A", "B"]],
                NIL.into()
            ),
            "B".into()
        );
    }

    #[test]
    fn test_ycombinator_ff() {
        // \f.(\x.\f(x x)) (\x.\f(x x))
        let y_combinator = list![
            Symbol::LAMBDA,
            list!["f"],
            list![
                list![Symbol::LAMBDA, list!["x"], list!["f", list!["x", "x"]]],
                list![Symbol::LAMBDA, list!["x"], list!["f", list!["x", "x"]]]
            ]
        ];

        let ff = list![
            Symbol::LAMBDA,
            list!["X"],
            list![
                Symbol::COND,
                list![list![ElementaryFunction::ATOM, "X"], "X"],
                list![
                    list![Symbol::QUOTE, T],
                    list!["FF", list![ElementaryFunction::CAR, "X"]]
                ]
            ]
        ];
        let argument = appq(list![list!["A", "B"], "C"].into());

        dbg!(eval(
            cons(ff.clone().into(), argument.clone().into()).into(),
            NIL.into()
        ));
        // ^ this should've returned "FF is unbound" message

        assert_eq!(
            handle_lambda(
                list![y_combinator, cons(ff.into(), argument.into())].into(),
                NIL.into()
            ),
            "A".into()
        );
    }
}

#[cfg(test)]
mod handle_label_tests {
    use super::*;

    #[test]
    fn test_ff() {
        let ff = list![
            Symbol::LABEL,
            "FF",
            list![
                Symbol::LAMBDA,
                list!["X"],
                list![
                    Symbol::COND,
                    list![list![ElementaryFunction::ATOM, "X"], "X"],
                    list![
                        list![Symbol::QUOTE, T],
                        list!["FF", list![ElementaryFunction::CAR, "X"]]
                    ]
                ]
            ]
        ];

        let args = list![
            list![
                cons(Symbol::QUOTE.into(), "A".into()),
                cons(Symbol::QUOTE.into(), "B".into())
            ],
            cons(Symbol::QUOTE.into(), "C".into())
        ];

        assert_eq!(
            handle_label(list![ff.clone(), args], NIL.into()),
            "A".into()
        );
    }
}
