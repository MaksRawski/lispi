use log::error;
use regex::Regex;

use crate::{
    elementary_functions::cons,
    types::{Atom, ElementaryFunction, NullableList, SExpression, Symbol, NIL},
};

pub fn parse(s: &str) -> Option<SExpression> {
    if !check_parenthesis(s) {
        error!("Parenthesis count mismatch!");
        return None;
    };
    if s.starts_with('(') {
        parse_sexp(s)
    } else {
        parse_symbol(s).map(|atom| atom.into())
    }
}
fn check_parenthesis(s: &str) -> bool {
    s.chars().filter(|c| *c == '(').count() == s.chars().filter(|c| *c == ')').count()
}

fn parse_symbol(s: &str) -> Option<Atom> {
    parse_as_keyword(s).or_else(|| {
        parse_as_number(s).or_else(|| parse_as_string(s).or_else(|| parse_as_other_symbol(s)))
    })
}

fn parse_as_keyword(s: &str) -> Option<Atom> {
    match s.to_uppercase().as_str() {
        "QUOTE" => Some(Symbol::QUOTE.into()),
        "COND" => Some(Symbol::COND.into()),
        "LAMBDA" => Some(Symbol::LAMBDA.into()),
        "LABEL" => Some(Symbol::LABEL.into()),

        "ATOM" => Some(ElementaryFunction::ATOM.into()),
        "CAR" => Some(ElementaryFunction::CAR.into()),
        "CDR" => Some(ElementaryFunction::CDR.into()),
        "CONS" => Some(ElementaryFunction::CONS.into()),
        "EQ" => Some(ElementaryFunction::EQ.into()),
        _ => None,
    }
}

fn parse_as_number(s: &str) -> Option<Atom> {
    s.parse::<i32>().map_or_else(
        |_| s.parse::<f64>().map(|f| f.into()).ok(),
        |i| Some(i.into()),
    )
}

fn parse_as_string(s: &str) -> Option<Atom> {
    if s.chars().filter(|c| *c == '"').count() % 2 == 1 {
        error!("Uneven number of quote marks: {}", s);
        return None;
    }
    s.strip_prefix('"')?.strip_suffix('"').map_or_else(
        || {
            error!("Missing a quote mark at the end of: {}", s);
            None
        },
        |str| Some(Atom::String(str.to_string())),
    )
}

fn parse_as_other_symbol(s: &str) -> Option<Atom> {
    if s.chars().filter(|c| *c == '"').count() == 0 && s.split_whitespace().count() == 1 {
        Some(Atom::Symbol(Symbol::Other(s.to_string())))
    } else {
        error!("A symbol cannot contain spaces or quotes: {}", s);
        None
    }
}

fn parse_sexp(s: &str) -> Option<SExpression> {
    match s
        .strip_prefix('(')
        .and_then(|no_prefix| no_prefix.strip_suffix(')'))
    {
        Some(symbols_str) => {
            let re = Regex::new(r#"".*?"|\(.*?\)|\w+"#).unwrap();
            let mut sexps: Vec<SExpression> = Vec::new();

            for capture in re.captures_iter(symbols_str) {
                let symbol = capture.get(0).unwrap().as_str();
                if symbol.starts_with('(') {
                    sexps.push(parse_sexp(symbol)?);
                } else {
                    sexps.push(parse_symbol(symbol)?.into());
                }
            }

            Some(iter_to_lisp_list(sexps.iter()).into())
        }
        None => {
            error!(
                "Invalid S-expression: {}. S-expressions must be enclosed in parenthesis.",
                s
            );
            None
        }
    }
}

fn iter_to_lisp_list<'a, I>(iter: I) -> NullableList
where
    I: IntoIterator<Item = &'a SExpression>,
{
    let sexps: Vec<SExpression> = iter.into_iter().cloned().collect();
    match sexps.len() {
        0 => NIL.into(),
        1 => cons(sexps.get(0).unwrap().clone(), NIL).into(),
        _ => cons(
            sexps.get(0).unwrap().clone(),
            iter_to_lisp_list(sexps.iter().skip(1)),
        )
        .into(),
    }
}

#[cfg(test)]
mod test_parser {
    use crate::list;

    use super::*;

    #[test]
    fn test_parse_atomic_symbol() {
        assert_eq!(parse("1"), Some(1.into()));
        assert_eq!(parse("1.23"), Some(1.23.into()));
        assert_eq!(parse("123456789"), Some(123456789.into()));
        assert_eq!(parse("3.141592653589"), Some(3.141592653589.into()));
        assert_eq!(parse("\"1\""), Some("1".into()));
        assert_eq!(parse("\"1.23\""), Some("1.23".into()));
        assert_eq!(parse("A"), Some(Symbol::Other("A".to_string()).into()));
        assert_eq!(parse("\"A\""), Some(Atom::String("A".to_string()).into()));
        assert_eq!(parse("\"A"), None);
        assert_eq!(parse("A\""), None);
        assert_eq!(
            parse("\"A B\""),
            Some(Atom::String("A B".to_string()).into())
        );
    }
    #[test]
    fn test_args() {
        assert_eq!(
            parse("(A)"),
            Some(list![Symbol::Other("A".to_string())].into())
        );
        assert_eq!(
            parse("(A B)"),
            Some(
                list![
                    Symbol::Other("A".to_string()),
                    Symbol::Other("B".to_string())
                ]
                .into()
            )
        );
        assert_eq!(
            parse("(A B C)"),
            Some(
                list![
                    Symbol::Other("A".to_string()),
                    Symbol::Other("B".to_string()),
                    Symbol::Other("C".to_string())
                ]
                .into()
            )
        );
        assert_eq!(
            parse("(A \"B\")"),
            Some(
                list![
                    Symbol::Other("A".to_string()),
                    Atom::String("B".to_string())
                ]
                .into()
            )
        );
        assert_eq!(
            parse(r#"(A "B" "C")"#),
            Some(
                list![
                    Symbol::Other("A".to_string()),
                    Atom::String("B".to_string()),
                    Atom::String("C".to_string())
                ]
                .into()
            )
        );
        // TODO: this should be a parsing error
        assert_eq!(
            parse(r#"("A" "B")"#),
            Some(list![Atom::String("A".to_string()), Atom::String("B".to_string())].into())
        );
    }
    #[test]
    fn test_function_parsing() {
        assert_eq!(
            parse("(atom)"),
            Some(list![ElementaryFunction::ATOM].into())
        );
        assert_eq!(parse("(car)"), Some(list![ElementaryFunction::CAR].into()));
        assert_eq!(parse("(cdr)"), Some(list![ElementaryFunction::CDR].into()));
        assert_eq!(
            parse("(cons)"),
            Some(list![ElementaryFunction::CONS].into())
        );
        assert_eq!(parse("(eq)"), Some(list![ElementaryFunction::EQ].into()));
    }
    #[test]
    fn test_fun_with_args_parsing() {
        assert_eq!(
            parse("(atom 1)"),
            Some(list![ElementaryFunction::ATOM, 1].into())
        );
        assert_eq!(
            parse("(atom \"1\")"),
            Some(list![ElementaryFunction::ATOM, Atom::String("1".to_string())].into())
        );
        assert_eq!(
            parse("(atom x)"),
            Some(list![ElementaryFunction::ATOM, Symbol::Other("x".to_string())].into())
        );
        assert_eq!(
            parse("(atom \"x\")"),
            Some(list![ElementaryFunction::ATOM, Atom::String("x".to_string())].into())
        );
        assert_eq!(
            parse("(cons 1 2)"),
            Some(list![ElementaryFunction::CONS, 1, 2].into())
        );
        assert_eq!(
            parse("(cons \"1\" 2)"),
            Some(list![ElementaryFunction::CONS, Atom::String("1".to_string()), 2].into())
        );
        assert_eq!(
            parse("(cons \"1\" \"2\")"),
            Some(
                list![
                    ElementaryFunction::CONS,
                    Atom::String("1".to_string()),
                    Atom::String("2".to_string())
                ]
                .into()
            )
        );
    }
    #[test]
    fn test_nested_sexps() {
        assert_eq!(
            parse("(cons 1 (cons 2 3))"),
            Some(
                list![
                    ElementaryFunction::CONS,
                    1,
                    list![ElementaryFunction::CONS, 2, 3]
                ]
                .into()
            )
        );
    }
}
