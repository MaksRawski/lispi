use crate::{
    elementary_functions::{car, cdr},
    list_functions::assoc_v,
    types::{Atom, NullableList, SExpression, SpecialForm, Symbol, NIL},
};

use super::keywords_glue::*;

// /// The universal lisp function AKA the interpreter:
// /// applies a function f to a list of arguments x
// pub fn apply(f: SExpression, x: List) -> Option<SExpression> {
//     eval(cons(f, appq(x.into())).into(), NIL.into())
// }

// /// applies QUOTE to each symbol in an expression
// pub(crate) fn appq(m: SExpression) -> SExpression {
//     if null(m.clone()) {
//         return NIL.into();
//     }
//     match m {
//         SExpression::List(l) => cons(appq(car(l.clone())), appq(cdr(l))).into(),
//         SExpression::Atom(Atom::Symbol(Symbol::ElementaryFunction(f))) => f.into(),
//         SExpression::Atom(a) => cons(SpecialForm::QUOTE, a).into(),
//     }
// }

/// evaluates an expression using association list `a`
pub fn eval(e: SExpression, a: NullableList) -> Option<SExpression> {
    match e {
        SExpression::Atom(e_atom) => match a {
            NullableList::List(a_list) => {
                Some(assoc_v(e_atom.clone(), a_list).unwrap_or(e_atom.into()))
            }
            NullableList::NIL => Some(e_atom.into()),
        },
        SExpression::List(e_list) => match car(e_list.clone()) {
            SExpression::Atom(car_e) => match car_e {
                Atom::Symbol(Symbol::SpecialForm(s)) => s.eval(e_list, a),
                Atom::Symbol(Symbol::ElementaryFunction(f)) => f.eval(e_list, a),
                Atom::Symbol(Symbol::LAMBDA) => Some(e_list.into()),
                Atom::Symbol(Symbol::LABEL) => {
                    log::error!("Invalid use of LABEL: {}", e_list);
                    None
                }
                Atom::Number(n) => {
                    log::error!("Tried to use {n} like a function: {}", e_list);
                    None
                }
                Atom::Bool(b) => {
                    log::error!("Tried to use {b:?} like a function: {}", e_list);
                    None
                }
                Atom::Symbol(Symbol::Other(s)) => handle_other_symbol(s, e_list, a),
            },
            SExpression::List(compound_func) => match car(compound_func.clone()) {
                SExpression::Atom(Atom::Symbol(Symbol::LABEL)) => handle_label(e_list, a),
                SExpression::Atom(Atom::Symbol(Symbol::LAMBDA)) => handle_lambda(e_list, a),
                SExpression::Atom(Atom::Symbol(Symbol::SpecialForm(SpecialForm::QUOTE))) => {
                    Some(cdr(compound_func))
                }
                SExpression::Atom(Atom::Number(_)) => {
                    log::error!("Tried to use a number as a function: {}", e_list);
                    None
                }
                SExpression::Atom(f) => {
                    if cdr(e_list.clone()) == NIL.into() {
                        eval(compound_func.into(), a)
                    } else {
                        log::error!("Tried to use {f} like LABEL or LAMBDA: {}", e_list);
                        None
                    }
                }
                SExpression::List(_) => {
                    log::error!("Tried to use a list as a function: {}", e_list);
                    None
                }
            },
        },
    }
}

// #[test]
// fn test_appq() {
//     use crate::list_macros::list;
//     assert_eq!(appq("A".into()), cons(SpecialForm::QUOTE, "A").into());
//     assert_eq!(appq(NIL.into()), NIL.into());
//     assert_eq!(
//         appq(ElementaryFunction::ATOM.into()),
//         ElementaryFunction::ATOM.into()
//     );
//     assert_eq!(
//         appq(list!["A", "B", "C"].into()),
//         list![
//             cons(SpecialForm::QUOTE, "A"),
//             cons(SpecialForm::QUOTE, "B"),
//             cons(SpecialForm::QUOTE, "C")
//         ]
//         .into()
//     );
//     assert_eq!(
//         appq(list!["A", list!["B", "C"]].into()),
//         list![
//             cons(SpecialForm::QUOTE, "A"),
//             list![cons(SpecialForm::QUOTE, "B"), cons(SpecialForm::QUOTE, "C")]
//         ]
//         .into()
//     );
// }

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
                    list![SpecialForm::QUOTE, list!["A", "B"]]
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
                    list![SpecialForm::QUOTE, list!["A", "B"]]
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
                    list![SpecialForm::QUOTE, "A"],
                    list![SpecialForm::QUOTE, "B"]
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
                    list![SpecialForm::QUOTE, list!["A", "B"]],
                    list![SpecialForm::QUOTE, "C"]
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
                    list![SpecialForm::QUOTE, "x"],
                    list![SpecialForm::QUOTE, "x"]
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
                    list![SpecialForm::QUOTE, "x"],
                    list![SpecialForm::QUOTE, "y"]
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
                    list![SpecialForm::QUOTE, list![1, 2, 3]]
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
                        list![SpecialForm::QUOTE, "A"],
                        list![SpecialForm::QUOTE, "B"]
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
                            list![SpecialForm::QUOTE, "A"],
                            list![SpecialForm::QUOTE, "B"]
                        ],
                        list![SpecialForm::QUOTE, "C"]
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
            handle_lambda(
                list![lambda_expr, list![SpecialForm::QUOTE, "A"]],
                NIL.into()
            ),
            Some("A".into())
        );
    }

    #[test]
    fn test_binds_lambda() {
        let lambda = list![Symbol::LAMBDA, list!["x", "y"], "x"];

        assert_eq!(
            handle_lambda(
                list![
                    lambda,
                    list![SpecialForm::QUOTE, "A"],
                    list![SpecialForm::QUOTE, "B"]
                ],
                NIL.into()
            ),
            Some("A".into())
        );

        let lambda = list![Symbol::LAMBDA, list!["x", "y"], "y"];
        assert_eq!(
            handle_lambda(
                list![
                    lambda,
                    list![SpecialForm::QUOTE, "A"],
                    list![SpecialForm::QUOTE, "B"]
                ],
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
                SpecialForm::COND,
                list![list![ElementaryFunction::EQ, "x", "y"], "x"],
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
                NIL.into()
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
                NIL.into()
            ),
            Some("A".into())
        );

        assert_eq!(
            handle_label(
                list![ff.clone(), list![SpecialForm::QUOTE, list!["A", "B"]]],
                NIL.into()
            ),
            Some("A".into())
        );

        assert_eq!(
            handle_label(
                list![ff, list![SpecialForm::QUOTE, list![list!["A", "B"], "C"]]],
                NIL.into()
            ),
            Some("A".into())
        );
    }
}
