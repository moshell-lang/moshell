use analyzer::importer::{ASTImporter, ImportResult, Imported};
use analyzer::name::Name;
use analyzer::resolve_all;
use analyzer::steps::typing::apply_types;
use ast::Expr;
use compiler::compile;
use context::source::{ContentId, Source};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use parser::parse_trusted;
use vm::execute_bytecode;

struct SingleImporter<'a>(Option<Expr<'a>>);

impl<'a> ASTImporter<'a> for SingleImporter<'a> {
    fn import(&mut self, _name: &Name) -> ImportResult<'a> {
        self.0
            .take()
            .map(|expr| Imported {
                content: ContentId(0),
                expr,
            })
            .into()
    }
}

fn prepare_bytecode(code: &str) -> Vec<u8> {
    let mut bytes = Vec::new();
    let expr = parse_trusted(Source::new(code, "test"));
    let mut resolve = resolve_all(Name::new("test"), &mut SingleImporter(Some(expr)));
    assert_eq!(resolve.diagnostics, &[]);
    let (engine, typing) = apply_types(
        &resolve.engine,
        &resolve.relations,
        &mut resolve.diagnostics,
    );
    assert_eq!(resolve.diagnostics, &[]);
    compile(&engine, &resolve.engine, &typing, &mut bytes).unwrap();
    bytes
}

fn criterion_benchmark(c: &mut Criterion) {
    let bytes = prepare_bytecode(
        "
        var u = 0
        var computing = true
        var v = 1
        while $computing {
            u = $u + 1
            v = $v + 2
            computing = $u != 20000
        }
    ",
    );
    c.bench_function("var", |b| {
        b.iter(|| unsafe { execute_bytecode(black_box(&bytes)) })
    });

    let bytes = prepare_bytecode(
        "
        fun fibonacci(n: Int) -> Int =
        if [ $n <= 1 ] {
            1
        } else {
            return fibonacci($n - 1) + fibonacci($n - 2)
        }
        fibonacci(25)
    ",
    );
    c.bench_function("fib 25", |b| {
        b.iter(|| unsafe { execute_bytecode(black_box(&bytes)) })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
