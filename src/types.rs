use std::fmt::Display;

use crate::elementary_functions::{car, cdr};

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
impl From<String> for Symbol {
    fn from(s: String) -> Self {
        Self::Other(s)
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
    Number(f64),
    String(String),
    Symbol(Symbol),
    Bool(Bool),
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
impl From<&str> for Atom {
    fn from(s: &str) -> Self {
        Atom::String(s.to_string())
    }
}
impl<T: Into<Symbol>> From<T> for Atom {
    fn from(s: T) -> Self {
        Atom::Symbol(s.into())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct List(pub Box<SExpression>, pub Box<SExpression>);

impl List {
    pub fn new(car: SExpression, cdr: SExpression) -> Self {
        Self(Box::new(car), Box::new(cdr))
    }
}

impl Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // match self {
        //     List(head, tail) => f.write_fmt(format_args!("({} . {})", head, tail)),
        // }
        let car_l = car(self.clone());
        match cdr(self.clone()) {
            SExpression::Atom(atom_cdr) => f.write_fmt(format_args!("({} . {})", car_l, atom_cdr)),
            SExpression::List(_) => f.write_fmt(format_args!("[{}]", disp_list(self.clone()))),
        }
    }
}

/// helper function for prettier displaying of lists
/// that is: display [1, 2, 3] instead of (1 . (2 . 3))
fn disp_list(l: List) -> String {
    let car_l = car(l.clone());
    match cdr(l) {
        SExpression::Atom(cdr_l) => match cdr_l {
            Atom::Bool(Bool::NIL) => format!("{}", car_l),
            _ => format!("{}, {}", car_l, cdr_l),
        },
        SExpression::List(cdr_l) => format!("{}, {}", car_l, disp_list(cdr_l)),
    }
}
#[test]
fn test_display_list() {
    use crate::list;
    assert_eq!(format!("{}", list![1]), "(1 . NIL)");
    assert_eq!(format!("{}", list![1, 2]), "(1 . 2)");
    assert_eq!(format!("{}", list![1, 2, 3]), "[1, 2, 3]");
    assert_eq!(format!("{}", list![1, 2, 3, 4]), "[1, 2, 3, 4]");

    assert_eq!(format!("{}", list![list![1, 2], 3]), "((1 . 2) . 3)");
    assert_eq!(format!("{}", list![list![1, 2], 3, 4]), "[(1 . 2), 3, 4]");
    assert_eq!(format!("{}", list![1, list![2, 3], 4]), "[1, (2 . 3), 4]");
    // TODO: how should we display sth like below?
    // dbg!(disp_list(list![list![1, 2], list![3, 4]]));
    // assert_eq!(
    //     format!("{}", list![list![1, 2], list![3, 4]]),
    //     "[(1 . 2) . (3 . 4)]"
    // );
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

#[derive(Clone)]
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

impl std::fmt::Debug for SExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpression::Atom(a) => f.write_fmt(format_args!("{}", a)),
            SExpression::List(l) => f.write_fmt(format_args!("{}", l)),
        }
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
