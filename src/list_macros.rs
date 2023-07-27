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
        cons(head, NIL.into())
    }};

    ( $head:expr, $tail:expr ) => {{
        use $crate::elementary_functions::*;
        use $crate::types::*;
        let head: SExpression = $head.into();
        let tail: SExpression = $tail.into();
        cons(head, cons(tail, NIL.into()).into())
    }};

    ( $head:expr, $($tail:expr),* ) => {{
        use $crate::elementary_functions::*;
        use $crate::types::*;
        let head: SExpression = $head.into();
        let tail: List = list!($($tail),*);
        cons(head, tail.into())
    }};
}

// because list is such a generic name and since `#[macro_export]` by default
// exports into the global scope, the line below is there to allow
// reffering to this macro by its path
pub use list;

#[test]
fn test_list_macro() {
    assert_eq!(list![T], cons(T.into(), NIL.into()));
    assert_eq!(
        list![T, T],
        cons(T.into(), cons(T.into(), NIL.into()).into()).into()
    );

    assert_eq!(
        list![1, 2, 3],
        cons(
            1.into(),
            cons(2.into(), cons(3.into(), NIL.into()).into()).into()
        )
    );

    let macro_list = list![1, list![2, 3], 4];
    let cons_list = cons(
        1.into(),
        cons(
            cons(2.into(), cons(3.into(), NIL.into()).into()).into(),
            cons(4.into(), NIL.into()).into(),
        )
        .into(),
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
/// assert_eq!(compose_car_cdr("cdar", list.clone()), Some(2.into()));
/// assert_eq!(compose_car_cdr("cadr", list.clone()), Some(3.into()));
/// assert_eq!(compose_car_cdr("cddr", list.clone()), Some(4.into()));
/// ```
pub fn compose_car_cdr(car_cdr_composition: &str, list: List) -> Option<SExpression> {
    if car_cdr_composition == "car" {
        return Some(car(list));
    } else if car_cdr_composition == "cdr" {
        return Some(cdr(list));
    }

    // skip the inner most car/cdr
    let next_composition = format!(
        "{}r",
        &car_cdr_composition
            .get(0..(car_cdr_composition.len() - 2))
            .unwrap_or_else(|| panic!("Invalid composition: {}", car_cdr_composition))
    );

    let next_list = if car_cdr_composition.ends_with("ar") {
        car(list.clone())
    } else if car_cdr_composition.ends_with("dr") {
        cdr(list.clone())
    } else {
        panic!("invalid composition: {}", car_cdr_composition);
    };

    if let SExpression::List(l) = next_list {
        return compose_car_cdr(&next_composition, l);
    } else {
        // we can't just panic here because this function will be called with
        // user provided input so when this function fails user should be provided
        // with a message appropriate to the situation
        //
        // those 2 lines below are just for deubgging purposes and probably
        // could (and should) be removed if all uses of this function
        // throught this crate have descriptive messages

        eprintln!("Can't {} atomic: {}", next_composition, next_list);
        dbg!(car_cdr_composition, list);

        return None;
    }
}
