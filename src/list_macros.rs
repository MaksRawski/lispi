use crate::elementary_functions::*;
use crate::types::*;

/// Abbreviation which allows one to write `list![e1, e2, ..., en]` instead of
/// `cons(e1, cons(e2, ..., cons(en, NIL)))`
///
/// requires `elementary_functions` and `types` to be in scope
#[macro_export]
macro_rules! list {
    ( $head:expr ) =>{{
        use $crate::elementary_functions::*;
        use $crate::types::*;
        let head: SExpression = $head.into();
        cons(head, NIL)
    }};

    ( $head:expr, $tail:expr ) => {{
        use $crate::elementary_functions::*;
        use $crate::types::*;
        let head: SExpression = $head.into();
        let tail: SExpression = $tail.into();
        cons(head, tail)
    }};

    ( $head:expr, $($tail:expr),* ) => {{
        use $crate::elementary_functions::*;
        use $crate::types::*;
        let head: SExpression = $head.into();
        let tail: List = list!($($tail),*);
        cons(head, tail.into())
    }};
}

// - make list! macro accesible via its real path
// - #[macro_export] makes the macro accessible from the crate root
pub use list;

#[test]
fn test_list_macro() {
    assert_eq!(list![T], cons(T, NIL).into());
    assert_eq!(list![T, T], cons(T, T).into());

    assert_eq!(
        list![1, 2, 3],
        cons(1.into(), cons(2.into(), 3.into()).into())
    );

    let macro_list = list![1, list![2, 3], 4];
    let cons_list = cons(
        1.into(),
        cons(cons(2.into(), 3.into()).into(), 4.into()).into(),
    );
    assert_eq!(macro_list, cons_list);
}

/// Compose car and cdr functions and apply them to a list.
/// Example usage:
/// ```
/// use lisp_parser::list_macros::{compose_car_cdr, list};
///
/// let list = list![list![1,2], list![3,4]];
///
/// assert_eq!(compose_car_cdr("caar", list.clone()), Some(1.into()));
/// assert_eq!(compose_car_cdr("cadr", list.clone()), Some(2.into()));
/// assert_eq!(compose_car_cdr("cdar", list.clone()), Some(3.into()));
/// assert_eq!(compose_car_cdr("cddr", list.clone()), Some(4.into()));
/// ```
pub fn compose_car_cdr(car_cdr_composition: &str, list: List) -> Option<SExpression> {
    if car_cdr_composition == "car" {
        return Some(car(list));
    } else if car_cdr_composition == "cdr" {
        return Some(cdr(list));
    }

    // skip the top most car/cdr
    let next_composition = format!(
        "c{}",
        &car_cdr_composition.chars().skip(2).collect::<String>()
    );

    let next_list = if car_cdr_composition.starts_with("ca") {
        car(list.clone())
    } else if car_cdr_composition.starts_with("cd") {
        cdr(list.clone())
    } else {
        panic!("invalid composition: {}", car_cdr_composition);
    };

    if let SExpression::List(l) = next_list {
        return compose_car_cdr(&next_composition, l);
    } else {
        eprintln!("Can't {} atomic: {}", next_composition, next_list);
        return None;
    }
}
