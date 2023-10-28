use std::borrow::Cow::{self, Borrowed, Owned};

use rustyline::{
    highlight::Highlighter, validate::MatchingBracketValidator, Completer, Helper, Highlighter,
    Hinter, Validator,
};

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
pub struct MyHelper {
    #[rustyline(Validator)]
    brackets: MatchingBracketValidator,
    #[rustyline(Highlighter)]
    highlighter: MyMatchingBracketHighlighter,
}

impl MyHelper {
    pub fn new() -> Self {
        Self {
            brackets: MatchingBracketValidator::new(),
            highlighter: MyMatchingBracketHighlighter {},
        }
    }
}

struct MyMatchingBracketHighlighter;

#[inline]
fn is_cursor_on_bracket(line: &str, pos: usize) -> bool {
    match line.chars().nth(pos) {
        Some(c) => c == '(' || c == ')',
        None => false,
    }
}

impl Highlighter for MyMatchingBracketHighlighter {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        match find_nearest_bracket(line, pos) {
            Some(nearest_bracket_index) => {
                let nearest_bracket = line.chars().nth(nearest_bracket_index).unwrap();
                match find_matching_bracket(line, nearest_bracket_index) {
                    Ok(matching_bracket_index) => {
                        let mut line = line.to_owned();
                        let matching_bracket = line.chars().nth(matching_bracket_index).unwrap();

                        if matching_bracket_index > nearest_bracket_index {
                            line.replace_range(
                                matching_bracket_index..=matching_bracket_index,
                                &format!("\x1b[1;34m{}\x1b[0m", matching_bracket),
                            );
                            line.replace_range(
                                nearest_bracket_index..=nearest_bracket_index,
                                &format!("\x1b[1;34m{}\x1b[0m", nearest_bracket),
                            );
                        } else {
                            line.replace_range(
                                nearest_bracket_index..=nearest_bracket_index,
                                &format!("\x1b[1;34m{}\x1b[0m", nearest_bracket),
                            );
                            line.replace_range(
                                matching_bracket_index..=matching_bracket_index,
                                &format!("\x1b[1;34m{}\x1b[0m", matching_bracket),
                            );
                        }
                        Owned(line)
                    }
                    Err(true) => {
                        let mut line = line.to_owned();
                        line.replace_range(
                            nearest_bracket_index..=nearest_bracket_index,
                            &format!("\x1b[1;31m{}\x1b[0m", nearest_bracket),
                        );
                        Owned(line)
                    }
                    Err(false) => unreachable!(),
                }
            }
            None => Borrowed(line),
        }
    }

    fn highlight_char(&self, _line: &str, _pos: usize) -> bool {
        true
    }
}

/// Tries to find a matching bracket for the character under cursor.
///
/// If the cursor isn't on a bracket, it will return `Err(false)`.
/// If the cursor is on a bracket but there was no match, it will return `Err(true)`.
/// If the cursor is on a bracket and it finds a matching bracket, it will return `Ok(usize)`
/// with the index of the matching bracket.
pub(crate) fn find_matching_bracket(line: &str, cursor_pos: usize) -> Result<usize, bool> {
    let mut depth = 0;
    match line.chars().nth(cursor_pos).ok_or(false)? {
        '(' => {
            for (i, c) in line.chars().skip(cursor_pos + 1).enumerate() {
                if c == '(' {
                    depth += 1;
                } else if c == ')' {
                    if depth == 0 {
                        return Ok(cursor_pos + 1 + i);
                    } else {
                        depth -= 1;
                    }
                }
            }
            Err(true)
        }
        ')' => {
            for (i, c) in line.chars().rev().skip(line.len() - cursor_pos).enumerate() {
                if c == ')' {
                    depth += 1;
                } else if c == '(' {
                    if depth == 0 {
                        return Ok(cursor_pos - i - 1);
                    } else {
                        depth -= 1;
                    }
                }
            }
            Err(true)
        }
        _ => Err(false),
    }
}

#[test]
fn test_find_matching_bracket() {
    assert_eq!(find_matching_bracket("(()())", 0), Ok(5));
    assert_eq!(find_matching_bracket("(()())", 1), Ok(2));
    assert_eq!(find_matching_bracket("(()())", 2), Ok(1));
    assert_eq!(find_matching_bracket("(()())", 3), Ok(4));
    assert_eq!(find_matching_bracket("(()())", 4), Ok(3));
    assert_eq!(find_matching_bracket("(()())", 5), Ok(0));

    assert_eq!(find_matching_bracket("(ABC", 0), Err(true));
    assert_eq!(find_matching_bracket("ABC)", 3), Err(true));
    assert_eq!(find_matching_bracket("ABC", 0), Err(false));
    assert_eq!(find_matching_bracket("ABC", 5), Err(false));
}

#[test]
fn test_is_cursor_on_bracket() {
    assert!(is_cursor_on_bracket("(abc (def 123))", 0));
    assert!(!is_cursor_on_bracket("(abc (def 123))", 1));
    assert!(!is_cursor_on_bracket("(abc (def 123))", 4));
    assert!(is_cursor_on_bracket("(abc (def 123))", 5));
    assert!(is_cursor_on_bracket("(abc (def 123))", 13));
    assert!(is_cursor_on_bracket("(abc (def 123))", 14));
}

fn find_nearest_bracket(line: &str, pos: usize) -> Option<usize> {
    if is_cursor_on_bracket(line, pos) {
        Some(pos)
    } else {
        let mut i = 1;
        loop {
            if pos + i <= line.len() && is_cursor_on_bracket(line, pos + i) {
                return Some(pos + i);
            } else if pos as isize - i as isize >= 0 && is_cursor_on_bracket(line, pos - i) {
                return Some(pos - i);
            } else if pos + i > line.len() && pos.checked_sub(i).is_none() {
                return None;
            } else {
                i += 1;
            }
        }
    }
}

#[test]
fn test_find_nearest_bracket() {
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 0), Some(0));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 1), Some(0));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 2), Some(0));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 3), Some(5));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 4), Some(5));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 5), Some(5));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 6), Some(5));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 7), Some(9));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 8), Some(9));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 9), Some(9));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 10), Some(11));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 11), Some(11));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 12), Some(11));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 13), Some(15));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 14), Some(15));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 15), Some(15));
    assert_eq!(find_nearest_bracket("(abc (def) (123))", 16), Some(16));

    assert_eq!(find_nearest_bracket("abc", 0), None);
    assert_eq!(find_nearest_bracket("abc", 4), None);
    assert_eq!(find_nearest_bracket("abc () def", 0), Some(4));
    assert_eq!(find_nearest_bracket("abc () def", 9), Some(5));
}
