use log::error;

use crate::{
    elementary_functions::cons,
    types::{
        Atom, BuiltinFunc, ElementaryFunction, NullableList, SExpression, SpecialForm, Symbol, F,
        NIL, T,
    },
};

pub fn parse(s: &str) -> Option<SExpression> {
    if !check_parenthesis(s) {
        error!("Parenthesis count mismatch!");
        return None;
    };
    let s = &sanitize(s);
    if let Some(t) = find_invalid_token(s) {
        error!("Invalid token: {}", t);
        return None;
    }
    if s.starts_with('(') {
        parse_sexp(s)
    } else {
        parse_atom(s).map(|atom| atom.into())
    }
}

fn sanitize(s: &str) -> String {
    s.replace(',', " ")
        .lines()
        .filter(|l| !l.starts_with(';'))
        .collect()
}

fn check_parenthesis(s: &str) -> bool {
    s.chars().filter(|c| *c == '(').count() == s.chars().filter(|c| *c == ')').count()
}

fn find_invalid_token(s: &str) -> Option<char> {
    s.chars().find(|c| {
        !c.is_ascii_alphanumeric() && !['.', '(', ')', ' ', '-', '+', '\t', '\n', '\r'].contains(c)
    })
}

fn parse_atom(s: &str) -> Option<Atom> {
    parse_as_keyword(s).or_else(|| parse_as_number(s).or_else(|| parse_as_other_symbol(s)))
}

fn parse_as_keyword(s: &str) -> Option<Atom> {
    let s = s.to_uppercase();
    // programmer's manual sets a limit of max 3 compositions, that is:
    // CADAR is valid but CDADAR is not
    if s.starts_with('C') && s.ends_with('R') && s.len() > 3 && s.len() < 7 {
        return Some(ElementaryFunction::CarCdrComposition(s).into());
    }
    match s.as_str() {
        "QUOTE" => Some(SpecialForm::QUOTE.into()),
        "COND" => Some(SpecialForm::COND.into()),
        "LAMBDA" => Some(SpecialForm::LAMBDA.into()),
        "LIST" => Some(SpecialForm::LIST.into()),
        "LABEL" => Some(SpecialForm::LABEL.into()),
        "DEFINE" => Some(SpecialForm::DEFINE.into()),
        "OR" => Some(SpecialForm::OR.into()),
        "AND" => Some(SpecialForm::AND.into()),

        "ATOM" => Some(ElementaryFunction::ATOM.into()),
        "CAR" => Some(ElementaryFunction::CAR.into()),
        "CDR" => Some(ElementaryFunction::CDR.into()),
        "CONS" => Some(ElementaryFunction::CONS.into()),
        "EQ" => Some(ElementaryFunction::EQ.into()),

        "EQUAL" => Some(BuiltinFunc::EQUAL.into()),
        "NULL" => Some(BuiltinFunc::NULL.into()),
        "NOT" => Some(BuiltinFunc::NOT.into()),
        "SUM" => Some(BuiltinFunc::SUM.into()),
        "PRDCT" => Some(BuiltinFunc::PRDCT.into()),
        "EXPT" => Some(BuiltinFunc::EXPT.into()),
        "TRACKLIST" => Some(BuiltinFunc::TRACKLIST.into()),
        "ERROR" => Some(BuiltinFunc::ERROR.into()),

        "T" => Some(T.into()),
        "F" => Some(F.into()),
        "NIL" => Some(NIL),
        _ => None,
    }
}

fn parse_as_number(s: &str) -> Option<Atom> {
    s.parse::<i32>().map_or_else(
        |_| s.parse::<f64>().map(|f| f.into()).ok(),
        |i| Some(i.into()),
    )
}

fn parse_as_other_symbol(s: &str) -> Option<Atom> {
    if s.chars().filter(|c| c.is_ascii_alphanumeric()).count() == s.len() {
        Some(Atom::Symbol(Symbol::Other(s.to_uppercase())))
    } else {
        error!("Not a valid symbol: {}", s);
        None
    }
}

fn parse_sexp(s: &str) -> Option<SExpression> {
    let s = s.replace('(', " ( ").replace(')', " ) ");
    let mut tokens = s.split_whitespace().peekable();

    parse_tokens_iter(&mut tokens)
}

fn parse_tokens_iter<'a, I>(tokens: &mut std::iter::Peekable<I>) -> Option<SExpression>
where
    I: Iterator<Item = &'a str>,
{
    if let Some(&token) = tokens.peek() {
        if token == "(" {
            let mut sexps: Vec<SExpression> = Vec::new();
            tokens.next();
            while tokens.peek() != Some(&")") {
                let parsed = parse_tokens_iter(tokens)?;
                sexps.push(parsed);
                tokens.next();
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
        assert_eq!(parse("ABC123"), Some("ABC123".into()));
        assert_eq!(parse("\"ABC123\""), None);
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

        assert_eq!(parse("(cond)"), Some(list![SpecialForm::COND].into()));
        assert_eq!(parse("(lambda)"), Some(list![SpecialForm::LAMBDA].into()));
        assert_eq!(parse("(label)"), Some(list![SpecialForm::LABEL].into()));
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
            parse("(atom x)"),
            Some(list![ElementaryFunction::ATOM, Symbol::Other("X".to_string())].into())
        );
        assert_eq!(
            parse("(atom (quote x))"),
            Some(
                list![
                    ElementaryFunction::ATOM,
                    list![SpecialForm::QUOTE, Symbol::Other("X".to_string())]
                ]
                .into()
            )
        );
        assert_eq!(
            parse("(cons 1 2)"),
            Some(list![ElementaryFunction::CONS, 1, 2].into())
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
                    list![SpecialForm::QUOTE, "A"],
                    list![SpecialForm::QUOTE, "B"]
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
                        list![SpecialForm::QUOTE, "A"],
                        list![SpecialForm::QUOTE, "B"]
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
        assert_eq!(parse("\""), None);
        assert_eq!(parse("A\""), None);
        assert_eq!(parse("\"A"), None);
    }
    #[test]
    fn test_sanitization() {
        assert_eq!(
            parse(";this is a comment\n(this,is,an,expression)"),
            Some(list!["THIS", "IS", "AN", "EXPRESSION"].into())
        );
    }
}
