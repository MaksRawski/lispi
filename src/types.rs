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

impl From<Symbol> for Atom {
    fn from(s: Symbol) -> Atom {
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

impl From<List> for SExpression {
    fn from(l: List) -> SExpression {
        SExpression::List(l)
    }
}

impl From<Atom> for SExpression {
    fn from(l: Atom) -> SExpression {
        SExpression::Atom(l)
    }
}

// some aliases for common types
pub const NIL: SExpression = SExpression::Atom(Atom::Symbol(Symbol::Nil));
pub const T: SExpression = SExpression::Atom(Atom::Symbol(Symbol::T));
