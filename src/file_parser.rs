use crate::elementary_functions::cdr;
use crate::parser::parse;
use crate::repl::find_matching_bracket;
use crate::types::{Atom, NullableList, SExpression, Symbol};
use crate::{interpreter::eval, list_macros::compose_car_cdr};
use std::{io::Write, ops::RangeInclusive};

// use crate::{interpreter::eval, parser::parse, types::NullableList};

/// returns (line, nth_char_in_line)
pub fn offset_to_coordinate(s: &str, offset: usize) -> Option<(usize, usize)> {
    let mut offset = offset;
    for (line_no, line) in s.lines().enumerate() {
        if offset < line.len() {
            return Some((line_no + 1, offset + 1));
        }
        offset -= line.len();
    }
    None // offset out of bounds
}

#[test]
fn test_offset_to_coordinate() {
    let s = "(1 2 3)\n(4 5 6)\n(7 8 9)";
    assert_eq!(offset_to_coordinate(s, 0), Some((1, 1)));
    assert_eq!(offset_to_coordinate(s, 6), Some((1, 7)));
    assert_eq!(offset_to_coordinate(s, 7), Some((2, 1)));
    assert_eq!(offset_to_coordinate(s, 13), Some((2, 7)));
    assert_eq!(offset_to_coordinate(s, 14), Some((3, 1)));
    assert_eq!(offset_to_coordinate(s, 20), Some((3, 7)));
    assert_eq!(offset_to_coordinate(s, 21), None);
}

fn extract_sexp_range(s: &str, offset: usize) -> Option<RangeInclusive<usize>> {
    let begin_of_sexp = s.chars().skip(offset).position(|c| c == '(')? + offset;
    match find_matching_bracket(s, begin_of_sexp) {
        Ok(end_of_sexp) => Some(begin_of_sexp..=end_of_sexp),
        Err(_) => {
            if let Some((line_no, char_no)) = offset_to_coordinate(s, begin_of_sexp) {
                log::error!("Unclosed parenthesis at {line_no}:{char_no}.");
            } else {
                todo!("offset out of bounds???")
            }
            None
        }
    }
}

pub(crate) fn split_sexps(s: &str) -> Option<Vec<&str>> {
    let mut sexps = Vec::new();
    let mut offset = 0;
    while let Some(skip) = s.chars().skip(offset).position(|c| c == '(') {
        let range = extract_sexp_range(s, offset + skip)?;
        offset = *range.end() + 1;
        sexps.push(&s[range]);
    }
    Some(sexps)
}

#[test]
fn test_extract_sexp() {
    assert_eq!(extract_sexp_range("123 (456) 789", 0), Some(4..=8));
    assert_eq!(extract_sexp_range("123 (456) 789", 4), Some(4..=8));
    assert_eq!(extract_sexp_range("123 (456) 789", 5), None);
    assert_eq!(extract_sexp_range("123 (456) 789", 14), None);
    assert_eq!(extract_sexp_range("123 456 789", 0), None);
}

#[test]
fn test_split_sexps() {
    assert_eq!(split_sexps("(1) (2) (3)"), Some(vec!["(1)", "(2)", "(3)"]));
    assert_eq!(split_sexps("(1 (2 (3)))"), Some(vec!["(1 (2 (3)))"]));
    assert_eq!(split_sexps("(1 (2)) (3)"), Some(vec!["(1 (2))", "(3)"]));
    assert_eq!(split_sexps(""), Some(vec![]));
    assert_eq!(split_sexps("(123"), None);
}

pub fn eval_file<W>(filename: &str, a_list: &mut NullableList, output: &mut W) -> Option<()>
where
    W: Write,
{
    let s = match std::fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            log::error!("Failed to open file {filename}: {e}");
            return None;
        }
    };
    let sexp_slices = split_sexps(&s)?;

    for slice in sexp_slices {
        // TODO: if there is error with parsing it would be nice to show the coordinates of where it occured
        let sexp = parse(slice)?;
        let (res, new_a_list) = eval(sexp, a_list.clone())?;
        *a_list = new_a_list;

        writeln!(output, "{}", res).unwrap();
    }
    Some(())
}

pub fn get_bound_symbols(a: &NullableList) -> Option<Vec<String>> {
    match a {
        NullableList::List(l) => match compose_car_cdr("caar", l.clone()) {
            Some(SExpression::Atom(Atom::Symbol(Symbol::Other(s)))) => {
                let mut v = vec![s];
                let rest = cdr(l.clone());
                match rest {
                    SExpression::List(rest) => {
                        v.append(&mut get_bound_symbols(&rest.into())?);
                        Some(v)
                    }
                    _ => Some(v),
                }
            }
            _ => todo!("invalid alist: {a:?}"),
        },
        NullableList::NIL => Some(vec![]),
    }
}

#[test]
fn test_get_bound_symbols() {
    use crate::list;
    assert_eq!(
        get_bound_symbols(&list![cons("first", ElementaryFunction::CAR)].into()),
        Some(vec!["first".to_string()])
    );
    assert_eq!(
        get_bound_symbols(
            &list![
                cons("head", ElementaryFunction::CAR),
                cons("tail", ElementaryFunction::CDR)
            ]
            .into()
        ),
        Some(vec!["head".to_string(), "tail".to_string()])
    );
}
