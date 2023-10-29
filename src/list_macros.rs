use crate::elementary_functions::*;
use crate::types::*;

/// Abbreviation which allows one to write `list![e1, e2, ..., en]` instead of
/// `cons(e1, cons(e2, ..., cons(en, NIL)))`
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
        cons(head, cons(tail, NIL))
    }};

    ( $head:expr, $($tail:expr),* ) => {{
        use $crate::elementary_functions::*;
        use $crate::types::*;
        let head: SExpression = $head.into();
        let tail: List = list!($($tail),*);
        cons(head, tail)
    }};
}

// because list is such a generic name and since `#[macro_export]` by default
// exports into the global scope, the line below is there to allow
// reffering to this macro by its path
pub use list;

#[test]
fn test_list_macro() {
    assert_eq!(list![T], cons(T, NIL));
    assert_eq!(list![T, T], cons(T, cons(T, NIL)));

    assert_eq!(list![1, 2, 3], cons(1, cons(2, cons(3, NIL))));

    let macro_list = list![1, list![2, 3], 4];
    let cons_list = cons(1, cons(cons(2, cons(3, NIL)), cons(4, NIL)));
    assert_eq!(macro_list, cons_list);
}

/// Compose car and cdr functions and apply them to a list.
/// Example usage:
/// ```
/// use lispi::list_macros::compose_car_cdr;
/// use lispi::elementary_functions::cons;
///
/// let c = cons(cons(1,2), cons(3,4));
///
/// assert_eq!(compose_car_cdr("caar", &c), Some(1.into()));
/// assert_eq!(compose_car_cdr("cdar", &c), Some(2.into()));
/// assert_eq!(compose_car_cdr("cadr", &c), Some(3.into()));
/// assert_eq!(compose_car_cdr("cddr", &c), Some(4.into()));
/// ```
pub fn compose_car_cdr(car_cdr_composition: &str, list: &List) -> Option<SExpression> {
    if car_cdr_composition.to_lowercase() == "car" {
        return Some(car(list.clone()));
    } else if car_cdr_composition.to_lowercase() == "cdr" {
        return Some(cdr(list.clone()));
    }

    // skip the inner most car/cdr
    let next_composition = format!(
        "{}r",
        &car_cdr_composition
            .get(0..(car_cdr_composition.len() - 2))
            .or_else(|| {
                log::error!("Tried to apply {} to: {}", car_cdr_composition, list);
                None
            })?
    );

    let next_list = if car_cdr_composition.to_lowercase().ends_with("ar") {
        car(list.clone())
    } else if car_cdr_composition.to_lowercase().ends_with("dr") {
        cdr(list.clone())
    } else {
        log::error!("Tried to apply {} to: {}", car_cdr_composition, list);
        return None;
    };

    if let SExpression::List(l) = next_list {
        compose_car_cdr(&next_composition, &l)
    } else {
        None
    }
}
