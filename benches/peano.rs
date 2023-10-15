use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use lispi::{interpreter::eval, parser::parse, types::NIL};

fn fac_10(c: &mut Criterion) {
    let fac = "(define (fac (lambda (n) (cond ((equal n 0) 1) (T (prdct n (fac (sum n -1))))))))";
    let fac_def = parse(fac).unwrap();
    let (_, a_list) = eval(fac_def, NIL.into()).unwrap();
    let fac_test = parse("(fac 10)").unwrap();

    c.bench_with_input(
        BenchmarkId::new("factorial_of_10", "fac_10"),
        &(fac_test, a_list),
        |b, (fac_test, a_list)| b.iter(|| eval(fac_test.clone(), a_list.clone())),
    );
}

criterion_group!(benches, fac_10);
criterion_main!(benches);
