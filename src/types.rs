use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum Symbol {
    T,
    Nil,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Symbol::T => f.write_fmt(format_args!("T")),
            Symbol::Nil => f.write_fmt(format_args!("Nil")),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
    Number(f64),
    String(String),
    Symbol(Symbol),
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Atom::Number(n) => f.write_fmt(format_args!("{}", n)),
            Atom::String(str) => f.write_fmt(format_args!("{:?}", str)),
            Atom::Symbol(s) => f.write_fmt(format_args!("{}", s)),
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
impl From<String> for Atom {
    fn from(s: String) -> Self {
        Atom::String(s.to_string())
    }
}

impl From<Symbol> for Atom {
    fn from(s: Symbol) -> Self {
        Atom::Symbol(s)
    }
}

#[derive(Clone, Debug)]
pub struct List(pub Box<SExpression>, pub Box<SExpression>);

impl List {
    pub fn new(car: SExpression, cdr: SExpression) -> Self {
        Self(Box::new(car), Box::new(cdr))
    }
}

impl Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            List(head, tail) => f.write_fmt(format_args!("({} · {})", head, tail)),
        }
    }
}

impl PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        let x = SExpression::List(self.clone());
        let y = SExpression::List(other.clone());

        x == y
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
        f.write_fmt(format_args!("{}", &self))
    }
}

impl From<List> for SExpression {
    fn from(l: List) -> SExpression {
        SExpression::List(l)
    }
}

impl<T: Into<Atom>> From<T> for SExpression {
    fn from(t: T) -> SExpression {
        SExpression::Atom(t.into())
    }
}

#[test]
fn test_sexp_diplay_implementation() {
    use crate::elementary_functions::cons;
    let expr: SExpression = cons(
        T,
        cons(cons((42.).into(), NIL).into(), "hello".into()).into(),
    )
    .into();

    assert_eq!(format!("{expr}"), "(T · ((42 · Nil) · \"hello\"))")
}

// some aliases for common types
pub const NIL: SExpression = SExpression::Atom(Atom::Symbol(Symbol::Nil));
pub const T: SExpression = SExpression::Atom(Atom::Symbol(Symbol::T));
