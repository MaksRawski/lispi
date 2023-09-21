use crate::{
    elementary_functions::{atom, car, cdr, cons},
    list_functions::{append, assoc_v, pair},
    list_macros::compose_car_cdr,
    recursive_functions::{equal, null},
    types::{Atom, Bool, ElementaryFunction, List, NullableList, SExpression, Symbol, NIL},
};

/// The universal lisp function AKA the interpreter:
/// applies a function f to a list of arguments x
pub fn apply(f: SExpression, x: List) -> SExpression {
    // TODO: look at appendix B of programmer's manual for how to allow usage
    // of other "compiled" functions?
    eval(cons(f, appq(x.into())).into(), NIL.into())
}

/// applies QUOTE to each symbol in an expression
fn appq(m: SExpression) -> SExpression {
    if null(m.clone()) {
        return NIL.into();
    }
    match m {
        SExpression::List(l) => cons(cons(Symbol::QUOTE, car(l.clone())), appq(cdr(l))).into(),
        SExpression::Atom(a) => cons(Symbol::QUOTE, a).into(),
    }
}

/// utility function for the monstrocity that is `eval`
fn handle_lambda(e: List, a: NullableList) -> SExpression {
    // e is of the form: ((LAMBDA, binds, expression) args)
    let binds: List = match compose_car_cdr("cadar", e.clone()) {
        Some(SExpression::List(l)) => l,
        _ => panic!(
            "Invalid use of LAMBDA: {}, second element should be a list of symbols to bind.",
            e
        ),
    };

    let expression = compose_car_cdr("cddar", e.clone()).unwrap_or_else(|| {
        panic!(
            "Invalid use of LAMBDA: {}, third element should be an S-expression.",
            e
        )
    });

    // evaluate the args before putting them into the expression
    let args: List = match cdr(e.clone()) {
        SExpression::Atom(arg) => cons(eval(arg.into(), a.clone()), NIL),
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
    // the entire e is of the form: ((LABEL, function_name, expression), args)

    let function_name: String = match compose_car_cdr("cadar", e.clone()) {
        Some(SExpression::Atom(Atom::Symbol(Symbol::Other(s))))
        | Some(SExpression::Atom(Atom::String(s))) => s,
        _ => panic!(
            "Invalid use of LABEL: {}, second element should be a function name (string).",
            e,
        ),
    };

    let expression = match compose_car_cdr("cddar", e.clone()) {
        Some(SExpression::Atom(a)) => todo!("Why do we have an atom? {}", a),
        Some(expr) => expr,
        None => todo!(),
    };
    let args = cdr(e.clone());

    let association_list = cons(function_name.clone(), car(e));

    let new_association_list = match a {
        NullableList::List(a_list) => cons(association_list, a_list),
        NullableList::NIL => association_list,
    };

    eval(cons(expression, args).into(), new_association_list.into())
}

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
                    SExpression::Atom(argument) => panic!("Invalid use of COND: {}, element should be an association list between predicates and S-expressions, but was: {}.", e_list, argument),
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
                        SExpression::Atom(argument) => panic!(
                            "Invalid use of CAR: {}, argument should be a list, but was: {}.",
                            e_list, argument
                        ),
                    },
                    ElementaryFunction::CDR => match eval(cdr(dbg!(e_list.clone())), a) {
                        SExpression::List(list) => cdr(dbg!(list)),
                        SExpression::Atom(argument) => panic!(
                            "Invalid use of CDR: {}, argument should be a list but was: {}.",
                            e_list, argument
                        ),
                    },
                    ElementaryFunction::CONS => match cdr(e_list.clone()) {
                        SExpression::List(arguments) => cons(eval(car(arguments.clone()), a.clone()), eval(cdr(arguments), a)).into(),
                        SExpression::Atom(_argument) => {
                            panic!("Invalid use of CONS: {}, two arguments are required.", e_list)
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
                        SExpression::List(arguments) =>  eval(
                            cons(
                                assoc_v(s.clone().into(), dbg!(a_list)).unwrap_or_else(|| {
                                    panic!("Invalid function: {}, symbol unbound.", s)
                                }),
                                eval(arguments.into(), a.clone().into()) // NOTE: paper claims that evlis should be used here
                            )
                            .into(),
                            a,
                        ),
                    },
                    NullableList::NIL => panic!("Invalid function: {}, symbol unbound.", s),
                },
            },
            SExpression::List(list_func) => match car(list_func.clone()){
                SExpression::Atom(Atom::Symbol(Symbol::LABEL)) =>  handle_label(e_list, a),
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
            SExpression::Atom(cdr_m_list) => {
                cons(eval(car(m_list), a.clone()), eval(cdr_m_list.into(), a)).into()
            }
            SExpression::List(cdr_m_list) => {
                cons(eval(car(m_list), a.clone()), evlis(cdr_m_list.into(), a)).into()
            }
        },
        NullableList::NIL => NIL.into(),
    }
}

#[test]
fn test_appq() {
    use crate::list_macros::list;
    assert_eq!(appq("A".into()), cons(Symbol::QUOTE, "A").into());
    assert_eq!(
        appq(cons("A", "B").into()),
        cons(cons(Symbol::QUOTE, "A"), cons(Symbol::QUOTE, "B")).into()
    );
    assert_eq!(
        appq(list!["A", "B", "C"].into()),
        list![
            cons(Symbol::QUOTE, "A"),
            cons(Symbol::QUOTE, "B"),
            cons(Symbol::QUOTE, "C")
        ]
        .into()
    );
    assert_eq!(
        appq(list![list!["A", "B"], "C"].into()),
        list![
            cons(Symbol::QUOTE, list!["A", "B"]),
            cons(Symbol::QUOTE, "C")
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
        assert_eq!(
            eval(
                list![ElementaryFunction::CAR, list![Symbol::QUOTE, "A", "B"]].into(),
                NIL.into()
            ),
            "A".into()
        )
    }
    #[test]
    fn test_eval_cdr() {
        assert_eq!(
            eval(
                cons(ElementaryFunction::CDR, cons(Symbol::QUOTE, cons("A", "B"))).into(),
                NIL.into()
            ),
            "B".into()
        )
    }
    #[test]
    fn test_eval_cons() {
        assert_eq!(
            eval(
                cons(
                    ElementaryFunction::CONS,
                    cons(cons(Symbol::QUOTE, "A"), cons(Symbol::QUOTE, "B"))
                )
                .into(),
                NIL.into()
            ),
            cons("A", "B").into()
        );
    }
    #[test]
    fn test_eval_cons_variables() {
        assert_eq!(
            eval(
                cons(ElementaryFunction::CONS, cons("A", "B")).into(),
                cons(cons("A", 1), cons("B", 2)).into()
            ),
            cons(1, 2).into()
        );
    }
    #[test]
    fn test_eval_eq_variables() {
        assert_eq!(
            eval(
                cons(
                    ElementaryFunction::EQ,
                    cons(cons(Symbol::QUOTE, "x"), cons(Symbol::QUOTE, "x"))
                )
                .into(),
                NIL.into()
            ),
            T.into()
        );
        assert_eq!(
            eval(
                cons(
                    ElementaryFunction::EQ,
                    cons(cons(Symbol::QUOTE, "x"), cons(Symbol::QUOTE, "y"))
                )
                .into(),
                NIL.into()
            ),
            NIL.into()
        );
        assert_eq!(
            eval(
                cons(ElementaryFunction::EQ, cons("x", "y")).into(),
                list![cons("y", "x")].into()
            ),
            T.into()
        );
    }
    #[test]
    fn test_eval_atom() {
        assert_eq!(
            eval(cons(ElementaryFunction::ATOM, "X").into(), NIL.into()),
            T.into()
        );
        assert_eq!(
            eval(
                cons(ElementaryFunction::ATOM, "X").into(),
                cons(cons("X", list![1, 2, 3]), NIL).into()
            ),
            NIL.into()
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
        let lambda_expr = cons(Symbol::LAMBDA, cons(list!["x"], "x"));
        let args = list![cons(Symbol::QUOTE, "A")];
        assert_eq!(
            handle_lambda(list![lambda_expr, args], NIL.into()),
            "A".into()
        );
    }

    #[test]
    fn test_binds_lambda() {
        let lambda = cons(Symbol::LAMBDA, cons(list!["x", "y"], "x"));
        let args = list![cons(Symbol::QUOTE, "A"), cons(Symbol::QUOTE, "B")];

        assert_eq!(
            handle_lambda(cons(lambda, args.clone()), NIL.into()),
            "A".into()
        );

        let lambda = cons(Symbol::LAMBDA, cons(list!["x", "y"], "y"));
        assert_eq!(handle_lambda(cons(lambda, args), NIL.into()), "B".into());
    }
    #[test]
    fn test_cond_lambda() {
        let binds = list!["x", "y"];
        let expression = cons(
            Symbol::COND,
            list![
                cons(cons(ElementaryFunction::EQ, cons("x", "y")), "x"),
                cons(cons(ElementaryFunction::ATOM, "x"), "y"),
                cons(T, cons(ElementaryFunction::CAR, "x"))
            ],
        );
        let lambda = cons(Symbol::LAMBDA, cons(binds.clone(), expression.clone()));

        let args = list![cons(Symbol::QUOTE, "A"), cons(Symbol::QUOTE, "B")];
        assert_eq!(
            handle_lambda(cons(lambda.clone(), args), NIL.into()),
            "B".into()
        );

        let args = list![cons(Symbol::QUOTE, "A"), cons(Symbol::QUOTE, "A")];
        assert_eq!(
            handle_lambda(cons(lambda.clone(), args), NIL.into()),
            "A".into()
        );

        let args = list![
            cons(
                ElementaryFunction::CONS,
                cons(cons(Symbol::QUOTE, "A"), cons(Symbol::QUOTE, "B"))
            ),
            cons(Symbol::QUOTE, "C")
        ];
        assert_eq!(handle_lambda(cons(lambda, args), NIL.into()), "A".into());
    }
}

#[cfg(test)]
mod handle_label_tests {
    // LABEL expressions are of the form: ((LABEL, function_name, expression), arg0, arg1, arg2, ...)
    use super::*;
    use crate::list_macros::list;
    #[test]
    fn test_ff() {
        let lambda = cons(
            Symbol::LAMBDA,
            cons(
                list!["X"],
                list![
                    Symbol::COND,
                    cons(cons(ElementaryFunction::ATOM, "X"), "X"),
                    cons(T, cons("FF", cons(ElementaryFunction::CAR, "X")))
                ],
            ),
        );
        let ff = cons(Symbol::LABEL, cons("FF", lambda));

        let arg = cons(Symbol::QUOTE, "A");
        assert_eq!(handle_label(list![ff.clone(), arg], NIL.into()), "A".into());

        let arg = list![
            ElementaryFunction::CONS,
            cons(Symbol::QUOTE, "A"),
            cons(Symbol::QUOTE, "B")
        ];
        assert_eq!(handle_label(list![ff.clone(), arg], NIL.into()), "A".into());

        let arg = list![
            ElementaryFunction::CONS,
            list![
                ElementaryFunction::CONS,
                cons(Symbol::QUOTE, "A"),
                cons(Symbol::QUOTE, "B")
            ],
            cons(Symbol::QUOTE, "C")
        ];
        assert_eq!(handle_label(list![ff.clone(), arg], NIL.into()), "A".into());
    }
}
