use std::fmt::{Debug, Display};

use crate::{
    elementary_functions::{car, cdr},
    interpreter::eval,
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Bool {
    T,
    NIL,
}
pub const T: Bool = Bool::T;
pub const NIL: Bool = Bool::NIL;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ElementaryFunction {
    CAR,
    CDR,
    CONS,
    EQ,
    ATOM,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Symbol {
    LAMBDA,
    LABEL,
    QUOTE,
    COND,
    ElementaryFunction(ElementaryFunction),
    Other(String),
}

impl From<ElementaryFunction> for Symbol {
    fn from(f: ElementaryFunction) -> Self {
        Symbol::ElementaryFunction(f)
    }
}
impl From<Bool> for Atom {
    fn from(b: Bool) -> Self {
        match b {
            Bool::T => Self::Bool(Bool::T),
            Bool::NIL => Self::Bool(Bool::NIL),
        }
    }
}

// NOTE: this leads to HUGE confusion
// consider using Symbol::Other and Atom::String directly
// in places where the difference is crucial
impl From<String> for Symbol {
    fn from(s: String) -> Self {
        Self::Other(s)
    }
}
impl From<&str> for Symbol {
    fn from(s: &str) -> Self {
        Self::Other(s.to_string())
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

#[derive(Clone, PartialEq)]
pub enum Atom {
    Number(f64),
    String(String),
    Symbol(Symbol),
    Bool(Bool),
}

impl std::fmt::Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Symbol(s) => f.write_fmt(format_args!("Symbol({})", s)),
            Atom::Number(n) => f.write_fmt(format_args!("{}", n)),
            Atom::String(s) => f.write_fmt(format_args!("String({:?})", s)),
            Atom::Bool(b) => f.write_fmt(format_args!("{:?}", b)),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Atom::Symbol(s) => f.write_fmt(format_args!("{}", s)),
            Atom::Number(n) => f.write_fmt(format_args!("{}", n)),
            Atom::String(s) => f.write_fmt(format_args!("{:?}", s)),
            Atom::Bool(b) => f.write_fmt(format_args!("{:?}", b)),
        }
    }
}

impl From<bool> for Atom {
    fn from(value: bool) -> Self {
        match value {
            true => T.into(),
            false => NIL.into(),
        }
    }
}

// TODO: replace these with generics
impl From<f64> for Atom {
    fn from(n: f64) -> Self {
        Atom::Number(n)
    }
}
impl From<i32> for Atom {
    fn from(n: i32) -> Self {
        Atom::Number(n.into())
    }
}
impl<T: Into<Symbol>> From<T> for Atom {
    fn from(s: T) -> Self {
        Atom::Symbol(s.into())
    }
}

#[derive(Clone, PartialEq)]
pub struct List(pub Box<SExpression>, pub Box<SExpression>);

impl List {
    pub fn new(car: SExpression, cdr: SExpression) -> Self {
        Self(Box::new(car), Box::new(cdr))
    }
}

impl Debug for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", debug_list(self, 0)))
    }
}

/// helper function for /proper/ displaying of List structures
/// Because List is both a type and a variant (of SExpression) it can lead to confusion:
/// sometimes you may see List with one item inside it (as a variant of SExpresion)
/// and sometimes with two items (as a tuple).
/// This function does pretty much the same thing as the one that could be derived from Debug,
/// but it won't display the List variant around List types.
fn debug_list(l: &List, indent_level: u8) -> String {
    let indent = "\t".repeat(indent_level.into());

    let mut s = format!("{indent}List");
    match car(l.clone()) {
        SExpression::Atom(car_l) => match cdr(l.clone()) {
            SExpression::Atom(cdr_l) => s.push_str(&format!("({}, {})", car_l, cdr_l,)),
            SExpression::List(cdr_l) => s.push_str(&format!(
                "(\n\t{indent}{},\n{}\n{indent})",
                car_l,
                debug_list(&cdr_l, indent_level + 1)
            )),
        },
        SExpression::List(car_l) => match cdr(l.clone()) {
            SExpression::Atom(cdr_l) => s.push_str(&format!(
                "(\n{},\n\t{indent}{}\n{indent})",
                debug_list(&car_l, indent_level + 1),
                cdr_l
            )),
            SExpression::List(cdr_l) => s.push_str(&format!(
                "(\n{},\n{}\n{indent})",
                debug_list(&car_l, indent_level + 1),
                debug_list(&cdr_l, indent_level + 1),
            )),
        },
    }
    s
}

impl Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", display_list(self, false)))
    }
}

/// helper function for prettier displaying of lists that is:
/// display [1, 2, 3] instead of (1 . (2 . (3 . NIL)))
/// but display: (1 . (2 . 3)) when the right-most element is not NIL
fn display_list(l: &List, is_cdr: bool) -> String {
    match car(l.clone()) {
        SExpression::Atom(car_l) => match cdr(l.clone()) {
            SExpression::Atom(cdr_l) => {
                if cdr_l == NIL.into() {
                    if is_cdr {
                        format!("{car_l}")
                    } else {
                        format!("[{car_l}]")
                    }
                } else {
                    format!("({car_l} . {cdr_l})")
                }
            }
            SExpression::List(cdr_l) => {
                if is_cons(&cdr_l) {
                    format!("({car_l} . {})", display_list(&cdr_l, false))
                } else if is_cdr {
                    format!("{car_l}, {}", display_list(&cdr_l, true))
                } else {
                    format!("[{car_l}, {}]", display_list(&cdr_l, true))
                }
            }
        },
        SExpression::List(car_l) => match cdr(l.clone()) {
            SExpression::Atom(cdr_l) => {
                if cdr_l == NIL.into() {
                    if is_cdr {
                        display_list(&car_l, false)
                    } else {
                        format!("[{}]", display_list(&car_l, false))
                    }
                } else {
                    format!("({} . {cdr_l})", display_list(&car_l, false))
                }
            }
            SExpression::List(cdr_l) => {
                if is_cons(&l.clone()) {
                    format!(
                        "({} . {})",
                        display_list(&car_l, false),
                        display_list(&cdr_l, false)
                    )
                } else if is_cdr {
                    format!(
                        "{}, {}",
                        display_list(&car_l, false),
                        display_list(&cdr_l, true),
                    )
                } else {
                    format!(
                        "[{}, {}]",
                        display_list(&car_l, false),
                        display_list(&cdr_l, true)
                    )
                }
            }
        },
    }
}

/// determines if a list is a _cons_ object that is:
/// it's not a list because the right-most element is not NIL
fn is_cons(l: &List) -> bool {
    match cdr(l.clone()) {
        SExpression::Atom(a) => a != NIL.into(),
        SExpression::List(cdr_l) => is_cons(&cdr_l),
    }
}

#[test]
fn test_display_list() {
    use crate::elementary_functions::cons;
    use crate::list;
    assert_eq!(format!("{}", cons(1, 2)), "(1 . 2)");
    assert_eq!(format!("{}", cons(1, cons(2, 3))), "(1 . (2 . 3))");
    assert_eq!(format!("{}", cons(cons(1, 2), 3)), "((1 . 2) . 3)");
    assert_eq!(
        format!("{}", cons(cons(1, 2), cons(3, 4))),
        "((1 . 2) . (3 . 4))"
    );

    assert_eq!(format!("{}", list![1]), "[1]");
    assert_eq!(format!("{}", list![1, 2]), "[1, 2]");
    assert_eq!(format!("{}", list![1, 2, 3]), "[1, 2, 3]");
    assert_eq!(format!("{}", list![1, 2, 3, 4]), "[1, 2, 3, 4]");

    assert_eq!(format!("{}", list![list![1, 2], 3]), "[[1, 2], 3]");
    assert_eq!(format!("{}", list![list![1, 2], 3, 4]), "[[1, 2], 3, 4]");
    assert_eq!(format!("{}", list![1, list![2, 3], 4]), "[1, [2, 3], 4]");
    assert_eq!(format!("{}", list![1, 2, list![3, 4]]), "[1, 2, [3, 4]]");
    assert_eq!(
        format!("{}", list![list![1, 2], list![3, 4]]),
        "[[1, 2], [3, 4]]"
    );
    assert_eq!(
        format!("{}", list![cons(1, 2), cons(3, 4)]),
        "[(1 . 2), (3 . 4)]"
    );
}

#[derive(Clone, Debug, PartialEq)]
pub enum NullableList {
    List(List),
    NIL,
}

impl From<List> for NullableList {
    fn from(l: List) -> Self {
        Self::List(l)
    }
}

impl From<Bool> for NullableList {
    fn from(b: Bool) -> Self {
        match b {
            Bool::NIL => Self::NIL,
            Bool::T => panic!("Tried to convert T into a NullableList!"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum SExpression {
    Atom(Atom),
    List(List),
}

impl PartialEq for SExpression {
    fn eq(&self, other: &Self) -> bool {
        crate::recursive_functions::equal(self.clone(), other.clone())
    }
}

impl Display for SExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpression::Atom(a) => f.write_fmt(format_args!("{}", a)),
            SExpression::List(l) => f.write_fmt(format_args!("{}", l)),
        }
    }
}

impl SExpression {
    pub fn eval(self) -> Option<SExpression> {
        eval(self, NIL.into())
    }
}

impl From<List> for SExpression {
    fn from(l: List) -> SExpression {
        SExpression::List(l)
    }
}

impl From<NullableList> for SExpression {
    fn from(nl: NullableList) -> Self {
        match nl {
            NullableList::List(l) => l.into(),
            NullableList::NIL => NIL.into(),
        }
    }
}

impl<T: Into<Atom>> From<T> for SExpression {
    fn from(t: T) -> SExpression {
        SExpression::Atom(t.into())
    }
}
