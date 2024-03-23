use analyzer::analyze;
use analyzer::importer::{ASTImporter, ImportResult, Imported};
use analyzer::name::Name;
use analyzer::reef::{Externals, ReefId};
use analyzer::relations::SourceId;
use ast::Expr;
use compiler::externals::CompilerExternals;
use compiler::{compile_reef, CompilerOptions};
use context::source::ContentId;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use parser::parse_trusted;
use vm::execute_bytecode;

struct SingleImporter(Option<Expr>);

impl ASTImporter for SingleImporter {
    fn import(&mut self, _name: &Name) -> ImportResult {
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
    let expr = parse_trusted(code);
    let externals = Externals::default();
    let compiler_externals = CompilerExternals::default();
    let mut analyzer = analyze(
        Name::new("test"),
        &mut SingleImporter(Some(expr)),
        &externals,
    );
    assert_eq!(analyzer.take_diagnostics(), &[]);

    compile_reef(
        &analyzer.engine,
        &analyzer.resolution.relations,
        &analyzer.typing,
        &analyzer.resolution.engine,
        &externals,
        &compiler_externals,
        ReefId(1),
        SourceId(0),
        &mut bytes,
        CompilerOptions::default(),
    )
    .unwrap();
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
