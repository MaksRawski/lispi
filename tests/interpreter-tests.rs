// NOTE: Some of these tests are identical to the ones in src/interpreter.rs
// but these ones make sure that the function `apply` applies them correctly.
#[cfg(test)]
mod apply_elementary_functions_tests {
    use lisp_interpreter::{
        elementary_functions::cons,
        interpreter::apply,
        list,
        types::{ElementaryFunction, NIL, T},
    };

    #[test]
    fn test_car() {
        assert_eq!(
            apply(ElementaryFunction::CAR.into(), list![cons(1, 2)]),
            1.into()
        )
    }
    #[test]
    fn test_cdr() {
        assert_eq!(
            apply(ElementaryFunction::CDR.into(), list![cons(1, 2)]),
            2.into()
        )
    }
    #[test]
    fn test_cons() {
        assert_eq!(
            apply(ElementaryFunction::CONS.into(), cons(1, 2).into()),
            cons(1, 2).into()
        )
    }
    #[test]
    fn test_eq() {
        assert_eq!(
            apply(ElementaryFunction::EQ.into(), list!["x", "x"]),
            T.into()
        );
        assert_eq!(
            apply(ElementaryFunction::EQ.into(), list!["x", "y"]),
            NIL.into()
        );
    }
    #[test]
    fn test_atom() {
        assert_eq!(apply(ElementaryFunction::ATOM.into(), list![1]), T.into());
        assert_eq!(
            apply(ElementaryFunction::ATOM.into(), list![list![1, 2]]),
            NIL.into()
        );
    }
}
mod test_programs {
    use lisp_interpreter::{elementary_functions::cons, interpreter::apply, list, types::Symbol};
    #[test]
    fn test_ff() {
        let ff = cons(
            Symbol::LABEL,
            cons(
                "FF",
                cons(
                    Symbol::LAMBDA,
                    cons(
                        list!["X"],
                        list![
                            Symbol::COND,
                            cons(cons(ElementaryFunction::ATOM, "X"), "X"),
                            cons(T, cons("FF", cons(ElementaryFunction::CAR, "X")))
                        ],
                    ),
                ),
            ),
        );
        let arg = "A";
        assert_eq!(apply(ff.clone().into(), list![arg]), "A".into());

        let arg = cons("A", "B");
        assert_eq!(apply(ff.clone().into(), list![arg]), "A".into());

        let arg = cons(cons("A", "B"), "C");
        assert_eq!(apply(ff.clone().into(), list![arg]), "A".into());
    }
}
