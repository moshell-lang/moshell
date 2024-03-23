use criterion::{black_box, criterion_group, criterion_main, Criterion};
use parser::parse;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("sample_install", |b| {
        b.iter(|| black_box(parse(include_str!("sample_install.msh"))))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
