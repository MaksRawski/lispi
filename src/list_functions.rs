use crate::elementary_functions::*;
use crate::list_macros::compose_car_cdr;
use crate::list_macros::list;
use crate::recursive_functions::*;
use crate::types::*;

/// append x to y
pub fn append(x: SExpression, y: SExpression) -> SExpression {
    if null(x.clone()) {
        return y;
    }
    match x {
        SExpression::List(l) => cons(car(l.clone()), append(cdr(l), y).into()).into(),
        SExpression::Atom(a) => cons(a.into(), y).into(),
    }
}

/// predicate which checks if x occurs among elements of y
pub fn among(x: SExpression, y: SExpression) -> bool {
    match y {
        SExpression::Atom(_) => !null(y),
        SExpression::List(l) => equal(x.clone(), car(l.clone())) || among(x, cdr(l)),
    }
}

/// returns list of pairs of corresponding elements of lists x and y
pub fn pair(x: SExpression, y: SExpression) -> SExpression {
    if null(x.clone()) && null(y.clone()) {
        return NIL;
    }
    match x {
        SExpression::Atom(ax) => match y {
            SExpression::Atom(ay) => cons(ax.into(), ay.into()).into(),
            SExpression::List(_ly) => panic!("not defined in the paper"),
        },
        SExpression::List(lx) => match y {
            SExpression::Atom(_ay) => panic!("not defined in the paper"),
            SExpression::List(ly) => cons(
                list![car(lx.clone()), car(ly.clone())].into(),
                pair(cdr(lx), cdr(ly)),
            )
            .into(),
        },
    }
}

/// if y is a list of the form ((u1, v1 ), ..., (un, vn)) and x
/// is one of the u’s, then assoc(x, y) is the corresponding v.
pub fn assoc(x: Atom, y: List) -> SExpression {
    // TODO: when x is not found in y, `compose_car_cdr` panics which
    // provides very unhelpful message: "Can't further car/cdr"
    let caar_y: Option<SExpression> = compose_car_cdr("caar", y.clone());

    // the paper mentions cadar but what they probably meant is cadr as cadar
    // doesn't even make sense in their example
    let cadr_y: Option<SExpression> = compose_car_cdr("cadr", y.clone());

    if let Some(SExpression::Atom(caar_y_atom)) = caar_y {
        if eq(caar_y_atom, x.clone()) {
            if let Some(some_cadr_y) = cadr_y {
                return some_cadr_y;
            } else {
                panic!("{x} is not in {y}");
            }
        } else {
            if let SExpression::List(cdr_y_list) = cdr(y) {
                return assoc(x, cdr_y_list);
            } else {
                panic!("Not a property list!");
            }
        }
    } else {
        panic!("Not a property list!");
    }
}

#[test]
fn test_append() {
    // append [(A, B); (C, D, E)] = (A, B, C, D, E)
    // we will set:
    // x = (A, B),
    // y = (C, D, E)
    let x: SExpression = cons((1.).into(), (2.).into()).into();
    let y: SExpression = cons((3.).into(), cons((4.).into(), (5.).into()).into()).into();

    // this is REALLY ugly but to create a macro that would create the list directly
    // we first need a _working_ append
    let res: SExpression = cons(
        (1.).into(),
        cons(
            (2.).into(),
            cons((3.).into(), cons((4.).into(), (5.).into()).into()).into(),
        )
        .into(),
    )
    .into();
    assert_eq!(append(x, y), res);
}

#[test]
fn test_among() {
    assert!(among((42.).into(), list![1., 2., 3., 42., NIL].into()));
    assert!(!among(T, list![1., 2., 3., 42., NIL].into()));
}

#[test]
fn test_pair() {
    assert_eq!(pair(T, T), list![T, T].into());

    // these 2 tests even though intuitively make sense (and would work with a python zip)
    // are undefined behaviour in LISP
    // assert_eq!(
    //     pair(T, list![1, 2, 3].into()),
    //     list![T, list![1, 2, 3]].into()
    // );

    // assert_eq!(
    //     pair(list![1, 2, 3].into(), T),
    //     list![list![1, 2, 3], T].into()
    // );

    // pair[(A, B, C); (X, (Y, Z), U)] = ((A, X), (B, (Y, Z)), (C, U)).
    let a: SExpression = "A".to_string().into();
    let b: SExpression = "B".to_string().into();
    let c: SExpression = "C".to_string().into();
    let x: SExpression = "X".to_string().into();
    let y: SExpression = "Y".to_string().into();
    let z: SExpression = "Z".to_string().into();
    let u: SExpression = "U".to_string().into();

    let input_x = list![a.clone(), b.clone(), c.clone()];
    let input_y = list![x.clone(), list![y.clone(), z.clone()], u.clone()];
    let output = pair(input_x.clone().into(), input_y.clone().into());
    dbg!(&output);

    let expected_output = list![list![a, x], list![b, list![y, z]], list![c, u]].into();
    dbg!(&expected_output);

    assert_eq!(output, expected_output);
}

#[test]
fn test_assoc() {
    // assoc[X; ( (W, (A, B)), (X, (C, D)), (Y, (E, F )) )] = (C, D).
    let a: SExpression = "A".into();
    let b: SExpression = "B".into();
    let c: SExpression = "C".into();
    let d: SExpression = "D".into();
    let e: SExpression = "E".into();
    let f: SExpression = "F".into();
    let w: SExpression = "W".into();
    let x: Atom = "X".into();
    let y: SExpression = "Y".into();

    let list = list![
        list![w, cons(a, b)],
        list![x.clone(), cons(c.clone(), d.clone())],
        list![y, cons(e, f)]
    ];
    dbg!(&list);
    assert_eq!(assoc(x, list), cons(c, d).into());
}
