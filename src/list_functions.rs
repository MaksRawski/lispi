use crate::elementary_functions::*;
use crate::list_macros::compose_car_cdr;
use crate::list_macros::list;
use crate::recursive_functions::*;
use crate::types::*;

/// append x to y
/// will return y if x is null
pub fn append(x: SExpression, y: SExpression) -> SExpression {
    if null(x.clone()) {
        return y;
    }
    match x {
        SExpression::List(l) => cons(car(l.clone()), append(cdr(l), y)).into(),
        SExpression::Atom(a) => cons(a, y).into(),
    }
}

/// predicate which checks if x occurs among elements of y
pub fn among(x: SExpression, y: SExpression) -> bool {
    match y {
        SExpression::Atom(a_y) => match x {
            SExpression::Atom(a_x) => eq(a_x, a_y),
            SExpression::List(_) => false,
        },
        SExpression::List(l) => equal(x.clone(), car(l.clone())) || among(x, cdr(l)),
    }
}

/// returns an association list: list of pairs of corresponding elements of x and y
/// x and y must be both of the same type: either both lists or both atoms
/// returns NIL only if both arguments are NIL
pub fn pair(x: SExpression, y: SExpression) -> NullableList {
    if null(x.clone()) && null(y.clone()) {
        return NIL.into();
    }
    match x {
        SExpression::Atom(atom_x) => match y {
            SExpression::Atom(atom_y) => cons(atom_x, atom_y).into(),
            SExpression::List(list_y) => {
                cons(atom_x, list_y).into()
                // unimplemented!(
                //     "Tried to pair an Atom with a List:\n{:?}\n{:?}",
                //     atom_x,
                //     list_y
                // )
            }
        },
        SExpression::List(list_x) => match y {
            SExpression::Atom(atom_y) => {
                cons(list_x, atom_y).into()
                // unimplemented!(
                //     "Tried to pair a List with an Atom:\n{:?}\n{:?}",
                //     list_x,
                //     atom_y
                // )
            }
            SExpression::List(list_y) => cons(
                cons(car(list_x.clone()), car(list_y.clone())),
                pair(cdr(list_x), cdr(list_y)),
            )
            .into(),
        },
    }
}

/// returns an association list:
/// list of pairs of corresponding elements of x and y appended to the list a
/// x and y must be both of the same type: either both lists or both atoms
///
/// in programmer's manual this is specified instead of the `pair` function
pub fn pairlis(x: SExpression, y: SExpression, a: List) -> List {
    if null(x.clone()) {
        return a;
    }
    match x {
        SExpression::Atom(atom_x) => match y {
            SExpression::Atom(atom_y) => cons(cons(atom_x, atom_y), a),
            SExpression::List(list_y) => {
                unimplemented!(
                    "Tried to pair an Atom with a List:\n{:?}\n{:?}",
                    atom_x,
                    list_y
                )
            }
        },
        SExpression::List(list_x) => match y {
            SExpression::Atom(atom_y) => {
                unimplemented!(
                    "Tried to pair a List with an Atom:\n{:?}\n{:?}",
                    list_x,
                    atom_y
                )
            }
            SExpression::List(list_y) => cons(
                list![car(list_x.clone()), car(list_y.clone())],
                pairlis(cdr(list_x), cdr(list_y), a),
            ),
        },
    }
}

/// If a is an association list, then assoc will produce the first pair whose
/// first term is x. Thus it's a table searching function.
///
/// In the original paper `assoc_v` was specified as `assoc`, but this version of `assoc`
/// taken from the programmer's manual just seems to be more convenient.
pub fn assoc(x: SExpression, a: List) -> Option<List> {
    if equal(compose_car_cdr("caar", &a)?, x.clone()) {
        match car(a.clone()) {
            SExpression::List(car_a) => Some(car_a),
            SExpression::Atom(_) => panic!("Invalid alist: {}", a),
        }
    } else {
        match cdr(a) {
            SExpression::List(cdr_a) => assoc(x, cdr_a),
            SExpression::Atom(_) => None,
        }
    }
}

/// If y is a list of the form `((u1, v1 ), ..., (un, vn))` and x
/// is one of the u’s, then `assoc(x, y)` is the corresponding v.
pub fn assoc_v(x: Atom, y: List) -> Option<SExpression> {
    if let SExpression::List(car_y) = car(y.clone()) {
        if let SExpression::Atom(caar_y) = car(car_y) {
            if eq(caar_y, x.clone()) {
                // the paper mentions cadar here but what they probably meant is cdar
                // as cadar doesn't even make sense in their example
                compose_car_cdr("cdar", &y).or_else(|| {
                    log::error!("No value is associated with {x} in {y}.");
                    None
                })
            } else if let SExpression::List(cdr_y_list) = cdr(y) {
                assoc_v(x, cdr_y_list)
            } else {
                None
            }
        } else {
            panic!("Invalid alist! Keys must be atomic, but there was: {y}");
        }
    } else {
        // HACK: this was NOT mentioned in the paper but allows
        // cons(u, v) to be a valid y
        assoc_v(x, cons(y, NIL))
    }
}

/// Auxiliary function for sublis.
/// Checks each pair from x, where x is of the form `((u1,v1), ..., (un,vn))`
/// and when u is equal to z then replaces it with a corresponding v.
/// Returns the z after all substitutions.
fn sub2(x: List, z: Atom) -> Atom {
    if let SExpression::List(car_x) = car(x.clone()) {
        if let SExpression::Atom(caar_x) = car(car_x.clone()) {
            if eq(caar_x, z.clone()) {
                match cdr(car_x) {
                    SExpression::Atom(cadr_x) => cadr_x,
                    SExpression::List(_cadr_x) => {
                        todo!("must v be atomic?");
                    }
                }
            } else {
                match cdr(x) {
                    SExpression::Atom(_cdr_x) => z,
                    SExpression::List(cdr_x) => sub2(cdr_x, z),
                }
            }
        } else {
            panic!("Invalid alist! There is no associated value for {x} in {z}.");
        }
    } else {
        // HACK: similar to what i've done in assoc:
        // allows cons(u, v) to be a valid x
        sub2(cons(x, NIL), z)
    }
}

/// Checks each pair from x, where x is of the form `((u1,v1), ..., (un,vn))`
/// and if a un is found in y then it's substituted for the corresponding vn.
///
/// Example:
/// ```common-lisp
/// (sublis '((1 . "one") (2 . "two") (3 . "three"))
///          '(3 2 1))
///         = ("three" "two" "one")
/// ```
pub fn sublis(x: List, y: SExpression) -> SExpression {
    match y {
        SExpression::Atom(ay) => sub2(x, ay).into(),
        SExpression::List(ly) => {
            cons(sublis(x.clone(), car(ly.clone())), sublis(x, cdr(ly))).into()
        }
    }
}

#[test]
fn test_append() {
    // append [(A, B); (C, D, E)] = (A, B, C, D, E)
    // we will set:
    // x = (A, B),
    // y = (C, D, E)
    let x: SExpression = cons(1., 2.).into();
    let y: SExpression = cons(3., cons(4., 5.)).into();

    // this is REALLY ugly but to create a macro that would create the list directly
    // we first need a _working_ append
    let res: SExpression = cons(1., cons(2., cons(3., cons(4., 5.)))).into();
    assert_eq!(append(x, y), res);
}

#[test]
fn test_among() {
    assert!(among((42.).into(), list![1, 2, 3, 42].into()));
    assert!(!among(T.into(), list![1, 2, 3, 42].into()));
}

#[test]
fn test_pair() {
    assert_eq!(pair(T.into(), T.into()), cons(T, T).into());

    // these 2 tests even though intuitively make sense (and would work with a python zip)
    // are undefined behaviour in LISP
    // assert_eq!(
    //     pair(T.into(), list![1, 2, 3].into()),
    //     list![T, list![1, 2, 3]].into()
    // );

    // assert_eq!(
    //     pair(list![1, 2, 3].into(), T.into()),
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
    let output = pair(input_x.into(), input_y.into());

    let expected_output = list![cons(a, x), cons(b, list![y, z]), cons(c, u)];

    assert_eq!(output, expected_output.into());
}

#[test]
fn test_pairlis() {
    assert_eq!(
        pairlis(
            list!["A", "B", "C"].into(),
            list!["U", "V", "W"].into(),
            list![list!["D", "X"], list!["E", "Y"]]
        ),
        list![
            list!["A", "U"],
            list!["B", "V"],
            list!["C", "W"],
            list!["D", "X"],
            list!["E", "Y"]
        ]
    );
}

#[test]
fn test_assoc() {
    assert_eq!(assoc_v(1.into(), list![cons(1, "one")]), Some("one".into()));
    assert_eq!(assoc_v(2.into(), list![cons(1, "one")]), None);
    assert_eq!(
        assoc_v(2.into(), list![cons(1, "one"), cons(2, "two")]),
        Some("two".into())
    );

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
        cons(w, cons(a, b)),
        cons(x.clone(), cons(c.clone(), d.clone())),
        cons(y, cons(e, f))
    ];

    assert_eq!(assoc_v(x, list), Some(cons(c, d).into()));
}

#[test]
fn test_sub2() {
    assert_eq!(sub2(list![cons(1, "one")], 1.into()), "one".into());
    assert_eq!(sub2(list![cons(1, "one")], 2.into()), 2.into());
    assert_eq!(
        sub2(list![cons(1, "one"), cons(2, "two")], 1.into()),
        "one".into()
    );

    assert_eq!(
        sub2(list![cons(1, "one"), cons(2, "two")], 2.into()),
        "two".into()
    );
    assert_eq!(
        sub2(list![cons(1, "one"), cons(2, "two")], 3.into()),
        3.into()
    );
}

#[test]
fn test_sublis() {
    assert_eq!(
        sublis(
            list![cons(1, "one"), cons(2, "two"), cons(3, "three")],
            list![1].into()
        ),
        list!["one"].into()
    );
    assert_eq!(
        sublis(
            list![cons(1, "one"), cons(2, "two"), cons(3, "three")],
            list![3, 2, 1].into()
        ),
        list!["three", "two", "one"].into()
    );
    assert_eq!(
        sublis(
            list![cons(1, "one"), cons(2, "two"), cons(3, "three")],
            list![3, 2, 42].into()
        ),
        list!["three", "two", 42].into()
    );
    assert_eq!(
        sublis(
            list![cons(1, "one"), cons(2, "two"), cons(3, "three")],
            list![4].into()
        ),
        list![4].into()
    );
}
