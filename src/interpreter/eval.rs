use crate::{
    elementary_functions::{car, cdr},
    list_functions::assoc_v,
    types::{Atom, NullableList, SExpression, Symbol, NIL},
};

use super::special_form_glue::*;

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

pub static mut TRACKLIST: Vec<Atom> = Vec::new();

// TODO: use Cow here for the returned `a`
/// evaluates an expression using association list `a` and returns the S-expression
/// and the new association list if `e` was evaluated succesfully
pub fn eval(e: SExpression, a: NullableList) -> Option<(SExpression, NullableList)> {
    match e {
        SExpression::Atom(e_atom) => match a.clone() {
            NullableList::List(a_list) => assoc_v(e_atom.clone(), a_list).map_or_else(
                || Some((e_atom.into(), a.clone())),
                |e| Some((e, a.clone())),
            ),
            NullableList::NIL => Some((e_atom.into(), a)),
        },
        SExpression::List(e_list) => match car(e_list.clone()) {
            SExpression::Atom(function) => {
                // accessing mutable statics is unsafe because it can cause undefined behaviour in mult-threaded applications
                // however since this entire project is single-threaded i consider this to be safe :)
                unsafe {
                    if TRACKLIST.contains(&function) {
                        log::info!("ENTERING {e_list}");
                    }
                }
                match function.clone() {
                    Atom::Symbol(Symbol::SpecialForm(s)) => s.eval(e_list.clone(), a),
                    Atom::Symbol(Symbol::ElementaryFunction(f)) => f.eval(e_list.clone(), a),
                    Atom::Symbol(Symbol::BuiltinFunc(f)) => f.eval(e_list.clone(), a),
                    Atom::Symbol(Symbol::Other(s)) => handle_other_symbol(s, e_list.clone(), a),
                    _ => {
                        log::error!("Tried to use {} like a function", e_list);
                        None
                    }
                }
                .map(|v| {
                    unsafe {
                        if TRACKLIST.contains(&function) {
                            log::info!("END OF {e_list}, VALUE IS\n{}", v.0);
                        }
                    }
                    v
                })
            }
            SExpression::List(compound_func) => match car(compound_func.clone()) {
                SExpression::Atom(Atom::Symbol(Symbol::SpecialForm(sf))) => sf.eval(e_list, a),
                SExpression::Atom(Atom::Number(_)) => {
                    log::error!("Tried to use a number as a function: {}", e_list);
                    None
                }
                SExpression::Atom(f) => {
                    if cdr(e_list.clone()) == NIL.into() {
                        eval(compound_func.into(), a)
                    } else {
                        log::error!("Tried to use {f} like special form: {}", e_list);
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
    use crate::elementary_functions::cons;
    use crate::list_macros::list;
    use crate::types::{F, T};

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
            Some(("A".into(), NIL.into()))
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
            Some((list!["B"].into(), NIL.into()))
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
            Some((cons("A", "B").into(), NIL.into()))
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
            Some((cons(list!["A", "B"], "C").into(), NIL.into()))
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
            Some((T.into(), NIL.into()))
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
            Some((F.into(), NIL.into()))
        );
        let a: NullableList = list![cons("x", 1), cons("y", 1)].into();
        assert_eq!(
            eval(list![ElementaryFunction::EQ, "x", "y"].into(), a.clone()),
            Some((T.into(), a))
        );
    }
    #[test]
    fn test_eval_atom() {
        assert_eq!(
            eval(list![ElementaryFunction::ATOM, "x"].into(), NIL.into()),
            Some((T.into(), NIL.into()))
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
            Some((F.into(), NIL.into()))
        );
        let a: NullableList = list![cons("x", list![1, 2, 3])].into();
        assert_eq!(
            eval(list![ElementaryFunction::ATOM, "x"].into(), a.clone()),
            Some((F.into(), a))
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
            Some((1.into(), NIL.into()))
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
            Some(("A".into(), NIL.into()))
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
            Some((cons("A", "B").into(), NIL.into()))
        );
    }
}

#[cfg(test)]
mod invalid_sexps_tests {
    use super::*;
    use crate::list;

    #[test]
    fn test_atom_as_func() {
        assert_eq!(eval(list![list![1]].into(), NIL.into()), None);
        assert_eq!(eval(list![list![T]].into(), NIL.into()), None);
        assert_eq!(eval(list![list![NIL]].into(), NIL.into()), None);
        assert_eq!(eval(list![list![1, 2]].into(), NIL.into()), None);
    }
}
