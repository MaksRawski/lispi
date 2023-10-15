use std::borrow::Cow::{self, Borrowed, Owned};

use rustyline::hint::HistoryHinter;
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
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
}

impl MyHelper {
    pub fn new() -> Self {
        Self {
            brackets: MatchingBracketValidator::new(),
            highlighter: MyMatchingBracketHighlighter {},
            hinter: HistoryHinter {},
        }
    }
}

struct MyMatchingBracketHighlighter;

#[inline]
fn other_bracket(bracket: char) -> char {
    if bracket == '(' {
        ')'
    } else {
        '('
    }
}

impl Highlighter for MyMatchingBracketHighlighter {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        let current_char = if let Some(c) = line.chars().nth(pos) {
            c
        } else {
            return Borrowed(line);
        };

        match find_matching_bracket(line, pos) {
            Ok(match_pos) => {
                let mut line = line.to_owned();
                if pos > match_pos {
                    line.replace_range(pos..=pos, &format!("\x1b[1;34m{}\x1b[0m", current_char));
                    line.replace_range(
                        match_pos..=match_pos,
                        &format!("\x1b[1;34m{}\x1b[0m", other_bracket(current_char)),
                    );
                } else {
                    line.replace_range(
                        match_pos..=match_pos,
                        &format!("\x1b[1;34m{}\x1b[0m", other_bracket(current_char)),
                    );
                    line.replace_range(pos..=pos, &format!("\x1b[1;34m{}\x1b[0m", current_char));
                }
                Owned(line)
            }
            Err(true) => {
                let mut line = line.to_owned();
                line.replace_range(pos..=pos, &format!("\x1b[1;31m{}\x1b[0m", current_char));
                Owned(line)
            }
            Err(false) => Borrowed(line),
        }
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        if let Some(c) = line.chars().nth(pos) {
            c == '(' || c == ')'
        } else {
            false
        }
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
