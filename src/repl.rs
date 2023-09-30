use std::borrow::Cow::{self, Borrowed, Owned};
use std::cell::Cell;

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
            highlighter: MyMatchingBracketHighlighter::new(),
        }
    }
}

struct MyMatchingBracketHighlighter {
    bracket: Cell<Option<(char, usize)>>,
}

impl MyMatchingBracketHighlighter {
    pub fn new() -> Self {
        Self {
            bracket: Cell::new(None),
        }
    }
}

impl Highlighter for MyMatchingBracketHighlighter {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        match self.bracket.get() {
            Some((c, idx)) => match find_matching_bracket(line, idx) {
                Ok((match_c, match_idx)) => {
                    let mut line = line.to_owned();
                    if idx > match_idx {
                        line.replace_range(idx..=idx, &format!("\x1b[1;34m{}\x1b[0m", c));
                        line.replace_range(
                            match_idx..=match_idx,
                            &format!("\x1b[1;34m{}\x1b[0m", match_c),
                        );
                    } else {
                        line.replace_range(
                            match_idx..=match_idx,
                            &format!("\x1b[1;34m{}\x1b[0m", match_c),
                        );
                        line.replace_range(idx..=idx, &format!("\x1b[1;34m{}\x1b[0m", c));
                    }
                    Owned(line)
                }
                Err(true) => {
                    let mut line = line.to_owned();
                    line.replace_range(idx..=idx, &format!("\x1b[1;31m{}\x1b[0m", c));
                    Owned(line)
                }
                Err(false) => Borrowed(line),
            },
            None => Borrowed(line),
        }
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        if let Some(c) = line.chars().nth(pos) {
            if c == '(' || c == ')' {
                self.bracket.set(Some((c, pos)));
            } else {
                self.bracket.set(None);
            }
        } else {
            self.bracket.set(None);
        }
        self.bracket.get().is_some()
    }
}

/// Tries to find a matching bracket for the character under cursor.
///
/// If the cursor isn't on a bracket, it will return `Err(false)`.
/// If the cursor is on a bracket but there was no match, it will return `Err(true)`.
/// If the cursor is on a bracket and it finds a matching bracket, it will return `Ok(usize)`
/// with the index of the matching bracket.
fn find_matching_bracket(line: &str, cursor_pos: usize) -> Result<(char, usize), bool> {
    let mut depth = 0;
    match line.chars().nth(cursor_pos).ok_or(false)? {
        '(' => {
            for (i, c) in line.chars().skip(cursor_pos + 1).enumerate() {
                if c == '(' {
                    depth += 1;
                } else if c == ')' {
                    if depth == 0 {
                        return Ok((c, cursor_pos + 1 + i));
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
                        return Ok((c, cursor_pos - i - 1));
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
    assert_eq!(find_matching_bracket("(()())", 0), Ok((')', 5)));
    assert_eq!(find_matching_bracket("(()())", 1), Ok((')', 2)));
    assert_eq!(find_matching_bracket("(()())", 2), Ok(('(', 1)));
    assert_eq!(find_matching_bracket("(()())", 3), Ok((')', 4)));
    assert_eq!(find_matching_bracket("(()())", 4), Ok(('(', 3)));
    assert_eq!(find_matching_bracket("(()())", 5), Ok(('(', 0)));

    assert_eq!(find_matching_bracket("(ABC", 0), Err(true));
    assert_eq!(find_matching_bracket("ABC)", 3), Err(true));
    assert_eq!(find_matching_bracket("ABC", 0), Err(false));
    assert_eq!(find_matching_bracket("ABC", 5), Err(false));
}
