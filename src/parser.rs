use log::error;

use crate::{
    elementary_functions::cons,
    types::{Atom, ElementaryFunction, NullableList, SExpression, Symbol, NIL, T},
};

pub fn parse(s: &str) -> Option<SExpression> {
    if !check_parenthesis(s) {
        error!("Parenthesis count mismatch!");
        return None;
    };
    if s.starts_with('(') {
        parse_sexp(s)
    } else {
        parse_atom(s).map(|atom| atom.into())
    }
}
fn check_parenthesis(s: &str) -> bool {
    s.chars().filter(|c| *c == '(').count() == s.chars().filter(|c| *c == ')').count()
}

fn parse_atom(s: &str) -> Option<Atom> {
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

        "T" => Some(T.into()),
        "NIL" => Some(NIL.into()),
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
    // according to the paper atom can only be a sequence of letters
    if s.chars().filter(|c| char::is_alphabetic(*c)).count() == s.len() {
        Some(Atom::Symbol(Symbol::Other(s.to_string())))
    } else {
        error!("Not a valid symbol: {}", s);
        None
    }
}

fn parse_sexp(s: &str) -> Option<SExpression> {
    let sraka: String = s.replace('(', " ( ").replace(')', " ) ");
    let tokens: Vec<&str> = sraka.split_whitespace().collect();

    parse_loop(&tokens, &mut 0)
}

// TODO: the signature here is kinda ugly, maybe use iterators??
fn parse_loop<'a>(tokens: &'a Vec<&'a str>, i: &mut usize) -> Option<SExpression> {
    let mut sexps: Vec<SExpression> = Vec::new();

    if let Some(&token) = tokens.get(*i) {
        if token == "(" {
            *i += 1;
            while tokens.get(*i) != Some(&")") {
                let parsed = parse_loop(tokens, i)?;
                sexps.push(parsed);
                *i += 1;
            }
            return Some(iter_to_lisp_list(sexps.iter()).into());
        } else {
            match parse_atom(token) {
                Some(atom) => Some(atom.into()),
                None => {
                    log::error!("Invalid token: {}", token);
                    None
                }
            }
        }
    } else {
        log::error!("Unexpected end of input!");
        None
    }
}

fn iter_to_lisp_list<'a, I>(mut iter: I) -> NullableList
where
    I: Iterator<Item = &'a SExpression>,
{
    match iter.next() {
        None => NIL.into(),
        Some(h) => cons(h.clone(), iter_to_lisp_list(iter)).into(),
    }
}

#[cfg(test)]
mod test_parser {
    use crate::list;

    use super::*;

    #[test]
    #[allow(clippy::approx_constant)]
    fn test_parse_atomic_symbol() {
        assert_eq!(parse("1"), Some(1.into()));
        assert_eq!(parse("1.23"), Some(1.23.into()));
        assert_eq!(parse("123456789"), Some(123456789.into()));
        assert_eq!(parse("3.141592653589"), Some(3.141592653589.into()));
        assert_eq!(parse("\"A\""), Some(Atom::String("A".to_string()).into()));
        assert_eq!(
            parse("\"A B C\""),
            Some(Atom::String("A B C".to_string()).into())
        );
        assert_eq!(parse("\"1\""), Some(Atom::String("1".into()).into()));
        assert_eq!(parse("\"1.23\""), Some(Atom::String("1.23".into()).into()));
        assert_eq!(parse("A"), Some(Symbol::Other("A".to_string()).into()));
        assert_eq!(parse("\"A"), None);
        assert_eq!(parse("A\""), None);
        assert_eq!(
            parse("\"A B\""),
            Some(Atom::String("A B".to_string()).into())
        );
    }
    #[test]
    fn test_parse_lists() {
        assert_eq!(parse("()"), Some(NIL.into()));
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
            parse("(A B (C) D)"),
            Some(
                list![
                    Symbol::Other("A".to_string()),
                    Symbol::Other("B".to_string()),
                    list![Symbol::Other("C".to_string())],
                    Symbol::Other("D".to_string())
                ]
                .into()
            )
        );
        assert_eq!(
            parse("((A B) (C) D)"),
            Some(
                list![
                    list![
                        Symbol::Other("A".to_string()),
                        Symbol::Other("B".to_string())
                    ],
                    list![Symbol::Other("C".to_string())],
                    Symbol::Other("D".to_string())
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
        // TODO: make sure that interpreter fails with "Tried to use string as a function"
        assert_eq!(
            parse(r#"("A" "B")"#),
            Some(list![Atom::String("A".to_string()), Atom::String("B".to_string())].into())
        );
    }
    #[test]
    fn test_keyword_parsing() {
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

        assert_eq!(parse("(cond)"), Some(list![Symbol::COND].into()));
        assert_eq!(parse("(lambda)"), Some(list![Symbol::LAMBDA].into()));
        assert_eq!(parse("(label)"), Some(list![Symbol::LABEL].into()));
        assert_eq!(parse("(T)"), Some(list![T].into()));
        assert_eq!(parse("(NIL)"), Some(list![NIL].into()));
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
            parse("(atom (quote x))"),
            Some(
                list![
                    ElementaryFunction::ATOM,
                    list![Symbol::QUOTE, Symbol::Other("x".to_string())]
                ]
                .into()
            )
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
        assert_eq!(
            parse("(cons (quote A) (quote B))"),
            Some(
                list![
                    ElementaryFunction::CONS,
                    list![Symbol::QUOTE, "A"],
                    list![Symbol::QUOTE, "B"]
                ]
                .into()
            )
        );
        assert_eq!(
            parse("(car (cons (quote A) (quote B)))"),
            Some(
                list![
                    ElementaryFunction::CAR,
                    list![
                        ElementaryFunction::CONS,
                        list![Symbol::QUOTE, "A"],
                        list![Symbol::QUOTE, "B"]
                    ]
                ]
                .into()
            )
        );
    }
    #[test]
    fn test_parser_edge_cases() {
        // env_logger::init();
        assert_eq!(parse("("), None);
        assert_eq!(parse(")"), None);
        assert_eq!(parse("()"), Some(NIL.into()));
        assert_eq!(parse(")("), None);
    }
}
