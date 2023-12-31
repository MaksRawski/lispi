use std::{
    borrow::Cow,
    borrow::Cow::*,
    fmt::{Debug, Display},
};

use crate::{
    elementary_functions::{car, cdr},
    interpreter::eval,
    list_macros::compose_car_cdr,
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Bool {
    T,
    F,
}
pub const T: Bool = Bool::T;
pub const F: Bool = Bool::F;
pub const NIL: Atom = Atom::NIL;

#[derive(Clone, PartialEq)]
pub enum ElementaryFunction {
    CAR,
    CDR,
    CarCdrComposition(String),
    CONS,
    EQ,
    ATOM,
}
impl Debug for ElementaryFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CarCdrComposition(c) => write!(f, "{c}"),
            Self::CAR => write!(f, "CAR"),
            Self::CDR => write!(f, "CDR"),
            Self::CONS => write!(f, "CONS"),
            Self::EQ => write!(f, "EQ"),
            Self::ATOM => write!(f, "ATOM"),
        }
    }
}

use crate::interpreter::elementary_fns_glue::*;
impl ElementaryFunction {
    pub fn eval(self, e_list: List, a: &NullableList) -> Option<(SExpression, &NullableList)> {
        match self {
            ElementaryFunction::ATOM => atom_fn(e_list, a).map(|e| (e, a)),
            ElementaryFunction::EQ => eq_fn(e_list, a).map(|e| (e, a)),
            ElementaryFunction::CAR => car_fn(e_list, a).map(|e| (e, a)),
            ElementaryFunction::CDR => cdr_fn(e_list, a).map(|e| (e, a)),
            ElementaryFunction::CONS => cons_fn(e_list, a).map(|e| (e, a)),
            ElementaryFunction::CarCdrComposition(c) => {
                car_cdr_composition(&c, e_list, a).map(|e| (e, a))
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SpecialForm {
    QUOTE,
    COND,
    AND,
    OR,
    LIST,
    PROG,
    LABEL,
    LAMBDA,
    DEFINE,
}

use crate::interpreter::special_form_glue::*;
impl SpecialForm {
    pub fn eval(
        self,
        e_list: List,
        a: &NullableList,
    ) -> Option<(SExpression, Cow<'_, NullableList>)> {
        match self {
            SpecialForm::QUOTE => handle_quote(e_list, a).map(|e| (e, Borrowed(a))),
            SpecialForm::COND => handle_cond(e_list, a).map(|e| (e, Borrowed(a))),
            SpecialForm::AND => handle_and(e_list, a).map(|e| (e, Borrowed(a))),
            SpecialForm::OR => handle_or(e_list, a).map(|e| (e, Borrowed(a))),
            SpecialForm::LABEL => handle_label(e_list, a).map(|e| (e, Borrowed(a))),
            SpecialForm::LAMBDA => handle_lambda(e_list, a).map(|e| (e, Borrowed(a))),
            SpecialForm::DEFINE => define_fn(e_list, a).map(|(e, a)| (e, Owned(a.into()))),
            SpecialForm::LIST => handle_list(e_list, a).map(|e| (e, Borrowed(a))),
            SpecialForm::PROG => todo!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BuiltinFunc {
    EQUAL,
    EQ1,
    APPEND,
    SUBST,
    SUBLIS,
    SASSOC,
    NULL,
    NOT,
    SUM,
    PRDCT,
    EXPT,
    TRACKLIST,
    ERROR,
    SELECT, // assoc_v
    CONC,   // technically a special form
}

use crate::interpreter::other_fns_glue::*;
impl BuiltinFunc {
    pub fn eval(self, e_list: List, a: &NullableList) -> Option<(SExpression, &NullableList)> {
        match self {
            BuiltinFunc::SUM => sum_fn(e_list, a).map(|e| (e, a)),
            BuiltinFunc::PRDCT => prdct_fn(e_list, a).map(|e| (e, a)),
            BuiltinFunc::EQUAL => equal_fn(e_list, a).map(|e| (e, a)),
            BuiltinFunc::EXPT => expt_fn(e_list, a).map(|e| (e, a)),
            BuiltinFunc::EQ1 => todo!(),
            BuiltinFunc::APPEND => todo!(),
            BuiltinFunc::SUBST => todo!(),
            BuiltinFunc::SUBLIS => todo!(),
            BuiltinFunc::SASSOC => todo!(),
            BuiltinFunc::SELECT => todo!(),
            BuiltinFunc::CONC => todo!(),
            BuiltinFunc::TRACKLIST => tracklist_fn(e_list).map(|e| (e.into(), a)),
            BuiltinFunc::ERROR => {
                match compose_car_cdr("cadr", &e_list) {
                    Some(arg) => match eval(arg, a) {
                        Some(v) => log::error!("(ERROR {})", v.0),
                        None => log::error!("{e_list}"),
                    },
                    None => log::error!("(ERROR)"),
                }
                None
            }
            BuiltinFunc::NULL => compose_car_cdr("cadr", &e_list).map(|arg| {
                let arg = eval(arg, a)?.0;
                Some(((arg == 0.into() || arg == NIL.into()).into(), a))
            })?,
            BuiltinFunc::NOT => compose_car_cdr("cadr", &e_list).map(|arg| {
                let arg = eval(arg, a)?.0;
                if arg == T.into() {
                    Some((F.into(), a))
                } else if arg == F.into() {
                    Some((T.into(), a))
                } else {
                    log::error!("Tried to NOT {arg}");
                    None
                }
            })?,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Symbol {
    ElementaryFunction(ElementaryFunction),
    SpecialForm(SpecialForm),
    BuiltinFunc(BuiltinFunc),
    Other(String),
}

impl From<ElementaryFunction> for Symbol {
    fn from(f: ElementaryFunction) -> Self {
        Symbol::ElementaryFunction(f)
    }
}
impl From<SpecialForm> for Symbol {
    fn from(f: SpecialForm) -> Self {
        Symbol::SpecialForm(f)
    }
}
impl From<BuiltinFunc> for Symbol {
    fn from(f: BuiltinFunc) -> Self {
        Symbol::BuiltinFunc(f)
    }
}

impl From<Bool> for Atom {
    fn from(b: Bool) -> Self {
        Self::Bool(b)
    }
}

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
        match self {
            Symbol::Other(s) => f.write_fmt(format_args!("{}", s)),
            Symbol::ElementaryFunction(e) => f.write_fmt(format_args!("{:?}", e)),
            Symbol::SpecialForm(sf) => f.write_fmt(format_args!("{:?}", sf)),
            Symbol::BuiltinFunc(bf) => f.write_fmt(format_args!("{:?}", bf)),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Atom {
    Symbol(Symbol),
    Number(f64),
    Bool(Bool),
    NIL,
}

impl std::fmt::Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Symbol(s) => f.write_fmt(format_args!("{:?}", s)),
            Atom::Number(n) => f.write_fmt(format_args!("{}", n)),
            Atom::Bool(b) => f.write_fmt(format_args!("{:?}", b)),
            Atom::NIL => f.write_fmt(format_args!("NIL")),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Atom::Symbol(s) => f.write_fmt(format_args!("{}", s)),
            Atom::Number(n) => f.write_fmt(format_args!("{}", n)),
            Atom::Bool(b) => f.write_fmt(format_args!("{:?}", b)),
            Atom::NIL => f.write_fmt(format_args!("NIL")),
        }
    }
}

impl From<bool> for Atom {
    fn from(value: bool) -> Self {
        match value {
            true => T.into(),
            false => F.into(),
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
                if cdr_l == NIL {
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
                if cdr_l == NIL {
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
        SExpression::Atom(a) => a != NIL,
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

impl From<Atom> for NullableList {
    fn from(atom: Atom) -> Self {
        match atom {
            Atom::NIL => Self::NIL,
            _ => panic!("Tried to convert {} into a NullableList", atom),
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
    pub fn eval(self, a: &NullableList) -> Option<SExpression> {
        eval(self, a).map(|(e, _a)| e)
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
