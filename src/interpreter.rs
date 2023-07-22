use crate::{
    elementary_functions::{atom, car, cdr, cons},
    list_functions::{append, assoc_v, pair},
    list_macros::compose_car_cdr,
    recursive_functions::equal,
    types::{Atom, ElementaryFunction, List, NullableList, SExpression, Symbol, NIL},
};

/// The universal lisp function AKA interpreter
/// applies function f to arguments x
pub fn apply(f: SExpression, x: List) -> SExpression {
    // TODO: look at appendix B for how to allow usage of other
    // "compiled" functions?
    eval(cons(f, appq(x.into()).into()).into(), NIL.into())
}

/// applies QUOTE to each symbol in an expression
pub fn appq(m: SExpression) -> List {
    match m {
        SExpression::List(l) => cons(
            cons(Symbol::QUOTE.into(), car(l.clone())).into(),
            appq(cdr(l)).into(),
        )
        .into(),
        SExpression::Atom(a) => cons(Symbol::QUOTE.into(), a.into()),
    }
}

/// utility function for the monstrocity that is `eval`
fn handle_lambda(e: List, a: NullableList) -> SExpression {
    // e is of the form: ((LAMBDA, binds, expression) args)
    let binds: List = match compose_car_cdr("cadar", e.clone()) {
        Some(SExpression::List(l)) => l,
        _ => unimplemented!(
            "Invalid use of LAMBDA: {}, second argument should be a list of symbols to bind.",
            e
        ),
    };

    let expression = compose_car_cdr("caddr", e.clone()).unwrap_or_else(|| {
        unimplemented!(
            "Invalid use of LAMBDA: {}, third argument should be an S-expression.",
            e
        )
    });

    // evaluate the args before putting them into the expression
    let args: List = match cdr(e.clone()) {
        SExpression::Atom(arg) => cons(eval(arg.into(), a.clone()), NIL.into()),
        SExpression::List(args) => match evlis(args.into(), a.clone()) {
            NullableList::List(evaluated_args) => evaluated_args,
            NullableList::NIL => unimplemented!(
                "Invalid use of LAMBDA: {}, Failed to eval the arguments.",
                e
            ),
        },
    };

    let bound_symbols = match pair(binds.into(), args.into()) {
        NullableList::List(l) => l,
        NullableList::NIL => unimplemented!(
            "Invalid use of LAMBDA: {}, no arguments to evaluate the lambda were provided.",
            e
        ),
    };

    let new_association_list = match append(bound_symbols.into(), a.clone().into()) {
        SExpression::List(l) => l,
        SExpression::Atom(_) => unreachable!("No bound symbols were found!"),
    };

    eval(expression, new_association_list.into())
}

/// utility function for the monstrocity that is `eval`
fn handle_label(e: List, a: NullableList) -> SExpression {
    // the entire e is of the form: ((LABEL, function_name, lambda), args)

    let function_name: String = match compose_car_cdr("cadar", e.clone()) {
        Some(SExpression::Atom(Atom::Symbol(Symbol::Other(s))))
        | Some(SExpression::Atom(Atom::String(s))) => s,
        _ => unimplemented!(
            "Invalid use of LABEL: {}, second argument should be a function name (string).",
            e
        ),
    };

    // 3rd argument should be a lambda which itself has a LAMBDA as the first argument
    let lambda = match compose_car_cdr("caddr", e.clone()) {
        Some(SExpression::List(l)) => match car(l.clone()) {
            SExpression::Atom(Atom::Symbol(Symbol::LAMBDA)) => l,
            _ => unimplemented!(
                "Invalid use of LABEL: {}, third argument should be a LAMBDA S-expression.",
                e
            ),
        },
        _ => unimplemented!(
            "Invalid use of LABEL: {}, third argument should be a LAMBDA S-expression.",
            e
        ),
    };
    let args = cdr(e);

    // TODO: TEST THIS: paper claims you should associate function name with
    // (LABEL, function_name, lambda) itself, which kinda feels too recursive?
    let new_association_list = match a {
        NullableList::List(a_list) => cons(
            cons(function_name.into(), lambda.clone().into()).into(),
            a_list.into(),
        ),
        NullableList::NIL => cons(function_name.into(), lambda.clone().into()),
    };

    eval(
        cons(lambda.into(), args).into(),
        new_association_list.into(),
    )
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
                    SExpression::List(cdr_e_list) => evcon(cdr_e_list, a),
                    SExpression::Atom(_) => unimplemented!("Invalid use of COND: {}, argument should be an association list between predicates and S-expressions.", e_list),
                }
                Atom::Symbol(Symbol::ElementaryFunction(f)) => match f {
                    ElementaryFunction::ATOM => atom(eval(cdr(e_list), a)).into(),
                    ElementaryFunction::EQ => match cdr(e_list) {
                        SExpression::List(arguments) => {
                            equal(eval(car(arguments.clone()), a.clone()), eval(cdr(arguments), a)).into()
                        }
                        SExpression::Atom(argument) => {
                            unimplemented!("Invalid use of EQ: {} two arguments are required.", argument);
                        }
                    },
                    ElementaryFunction::CAR => match eval(cdr(e_list.clone()), a) {
                        SExpression::List(list) => car(list),
                        SExpression::Atom(_argument) => unimplemented!(
                            "Invalid use of CAR: {}, argument should be a list.",
                            e_list
                        ),
                    },
                    ElementaryFunction::CDR => match eval(cdr(e_list.clone()), a) {
                        SExpression::List(list) => cdr(list),
                        SExpression::Atom(_argument) => unimplemented!(
                            "Invalid use of CDR: {}, argument should be a list.",
                            e_list
                        ),
                    },
                    ElementaryFunction::CONS => match cdr(e_list.clone()) {
                        SExpression::List(arguments) => cons(eval(car(arguments.clone()), a.clone()), eval(cdr(arguments), a)).into(),
                        SExpression::Atom(_argument) => {
                            unimplemented!("Invalid use of EQ: {} two arguments are required.", e_list)
                        }
                    },
                },
                Atom::Symbol(Symbol::LAMBDA) => unimplemented!("Invalid use of LAMBDA: {}", e_list),
                Atom::Symbol(Symbol::LABEL) => unimplemented!("Invalid use of LABEL: {}", e_list),
                Atom::Number(n) => unimplemented!("Invalid function: {}", n),
                Atom::Bool(b) => unimplemented!("Invalid function: {:?}", b),
                Atom::Symbol(Symbol::Other(s)) | Atom::String(s) => match a.clone() {
                    NullableList::List(a_list) => match cdr(e_list) {
                        SExpression::Atom(argument) => eval(
                            cons(
                                assoc_v(s.clone().into(), a_list).unwrap_or_else(|| {
                                    unimplemented!("Invalid function: {}, symbol unbound.", s)
                                }),
                                eval(argument.into(), a.clone()),
                            )
                            .into(),
                            a,
                        ),
                        SExpression::List(arguments) => eval(
                            cons(
                                assoc_v(s.clone().into(), a_list).unwrap_or_else(|| {
                                    unimplemented!("Invalid function: {}, symbol unbound.", s)
                                }),
                                evlis(arguments.into(), a.clone().into()).into()
                            )
                            .into(),
                            a,
                        ),
                    },
                    NullableList::NIL => unimplemented!("Invalid function: {}, symbol unbound.", s),
                },
            },
            SExpression::List(list_func) => match car(list_func.clone()){
                SExpression::Atom(Atom::Symbol(Symbol::LABEL)) => handle_label(e_list, a),
                SExpression::Atom(Atom::Symbol(Symbol::LAMBDA)) => handle_lambda(e_list, a),
                SExpression::Atom(Atom::Symbol(Symbol::QUOTE)) => cdr(list_func),
                SExpression::Atom(_func) => unimplemented!("Only LABEL and LAMBDA can be used this way: {}", list_func),
                SExpression::List(_) => unimplemented!("Tried to use a list as a function: {}", list_func)
            },
        },
    }
}

/// evaulates the propositional terms in order, and chooses
/// the form following the first true predicate
fn evcon(_c: List, _a: NullableList) -> SExpression {
    todo!()
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
    use crate::list_macros::list;
    assert_eq!(appq(1.into()), cons(Symbol::QUOTE.into(), 1.into()));
    assert_eq!(
        appq(cons(1.into(), 2.into()).into()),
        cons(
            cons(Symbol::QUOTE.into(), 1.into()).into(),
            cons(Symbol::QUOTE.into(), 2.into()).into()
        )
    );
    assert_eq!(
        appq(list![1, 2, 3].into()),
        list![
            cons(Symbol::QUOTE.into(), 1.into()),
            cons(Symbol::QUOTE.into(), 2.into()),
            cons(Symbol::QUOTE.into(), 3.into())
        ]
    );
}

#[cfg(test)]
mod interpreter_tests {
    use super::*;
    use crate::list_macros::list;

    #[test]
    fn test_apply_car() {
        assert_eq!(
            apply(
                ElementaryFunction::CAR.into(),
                cons(list![1, 2].into(), NIL.into())
            ),
            1.into()
        )
    }
    #[test]
    fn test_apply_cdr() {
        assert_eq!(
            apply(
                ElementaryFunction::CDR.into(),
                cons(list![1, 2].into(), NIL.into())
            ),
            2.into()
        )
    }
    #[test]
    fn test_apply_cons() {
        assert_eq!(
            apply(
                ElementaryFunction::CONS.into(),
                cons(1.into(), 2.into()).into()
            ),
            cons(1.into(), 2.into()).into()
        )
    }
}
