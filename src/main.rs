#![allow(dead_code)]
#[derive(Clone, Debug, PartialEq)]
enum Symbol {
    T,
    Nil,
}

#[derive(Clone, Debug, PartialEq)]
enum Atom {
    Number(f64),
    String(String),
    Symbol(Symbol),
}

trait Eval {
    type Value;
    fn eval(self) -> Self::Value;
}
impl Eval for Atom {
    type Value = Atom;

    fn eval(self) -> Self::Value {
        self
    }
}

#[derive(Clone, Debug)]
struct List(Box<SExpression>, Box<SExpression>);
impl List {
    pub fn car(self) -> SExpression {
        *self.0
    }
    pub fn cdr(self) -> SExpression {
        *self.1
    }
}

#[derive(Clone, Debug)]
enum SExpression {
    Atom(Atom),
    List(List),
}

fn cons(car: SExpression, cdr: SExpression) -> List {
    List(Box::new(car), Box::new(cdr))
}

/// The first atomic symbol of S-expression
fn ff(expr: SExpression) -> Atom {
    match expr {
        SExpression::Atom(a) => return a,
        SExpression::List(l) => return ff(l.car()),
    }
}

/// substitute with x all occurences of y in expression z
fn subst(x: SExpression, y: Atom, z: SExpression) -> SExpression {
    match z.clone() {
        SExpression::Atom(a) => {
            if a == y {
                return x;
            } else {
                return z;
            }
        }
        SExpression::List(l) => {
            return SExpression::List(cons(
                subst(x.clone(), y.clone(), l.clone().car()),
                subst(x, y, l.cdr()),
            ));
        }
    }
}

#[test]
fn test_ff() {
    const A: SExpression = SExpression::Atom(Atom::Number(1.));
    const B: SExpression = SExpression::Atom(Atom::Number(2.));
    const C: SExpression = SExpression::Atom(Atom::Number(3.));

    let ff_example_list = cons(SExpression::List(cons(A, B)), C);
    dbg!(ff_example_list.clone());

    let ff_result = ff(SExpression::List(ff_example_list));
    dbg!(ff_result.clone());

    assert_eq!(ff_result, Atom::Number(1.));
}
fn main() {}
