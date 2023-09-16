use std::path::PathBuf;

use analyzer::importer::{ASTImporter, ImportResult, StaticImporter};
use analyzer::name::Name;
use analyzer::reef::{Externals, Reef};
use analyzer::relations::SourceId;
use analyzer::types::hir::ExprKind;
use analyzer::types::ty::TypeRef;
use analyzer::{analyze, types, Analyzer, Inject};
use cli::pipeline::FileImporter;
use compiler::{compile, CompilerOptions};
use context::source::Source;
use parser::parse_trusted;
use vm::value::VmValue;
use vm::{VmError, VmValueFFI, VM};

struct Runner<'a> {
    current_page: usize,
    externals: Externals<'a>,
    vm: VM,
    analyzer: Analyzer<'a>,
}

impl Default for Runner<'_> {
    fn default() -> Self {
        let mut externals = Externals::default();
        let mut std_importer = FileImporter::new(PathBuf::from("../lib"));
        let mut vm = VM::default();

        let std_name = Name::new("std");
        let analyzer = analyze(std_name.clone(), &mut std_importer, &externals);
        let mut buff = Vec::new();

        compile(
            &analyzer.engine,
            &analyzer.typing,
            &analyzer.resolution.relations,
            &analyzer.resolution.engine,
            &externals,
            externals.current,
            SourceId(0),
            &mut buff,
            CompilerOptions::default(),
        )
        .expect("std did not compile successfully");

        vm.register(&buff).expect("VM std register");
        unsafe {
            vm.run().expect("VM std init");
        }

        externals.register(Reef::new("std".to_string(), analyzer));

        Self {
            current_page: 0,
            externals,
            vm,
            analyzer: Analyzer::default(),
        }
    }
}

// use an invalid name in moshell's language specs
const VAR_EXPR_STORAGE: &str = "7 ! ^";

impl Runner<'static> {
    fn eval(&mut self, expr: &'static str) -> VmValue {
        match self.try_eval(expr) {
            Ok(v) => v,
            Err(VmError::Panic) => panic!("VM did panic"),
            Err(VmError::Internal) => panic!("VM internal error"),
        }
    }

    fn try_eval(&mut self, expr: &'static str) -> Result<VmValue, VmError> {
        let name = Name::new("runner");
        let src = Source::unknown(expr);
        let mut importer = StaticImporter::new([(name.clone(), src)], parse_trusted);
        let ImportResult::Success(imported) = importer.import(&name) else {
            unreachable!()
        };

        let inject = Inject {
            name: name.clone(),
            imported,
            attached: if self.current_page == 0 {
                None
            } else {
                Some(SourceId(self.current_page - 1))
            },
        };

        let mut analysis = self.analyzer.inject(inject, &mut importer, &self.externals);
        let diagnostics = analysis.take_diagnostics();

        let reef = self.externals.current;

        if !diagnostics.is_empty() {
            panic!("input had analysis errors")
        }
        let mut bytes = Vec::new();

        let chunk = self
            .analyzer
            .engine
            .get_user(SourceId(self.current_page))
            .unwrap();
        let chunk_expr = &chunk.expression.as_ref().unwrap();

        let evaluated_expr_type = if let ExprKind::Block(exprs) = &chunk_expr.kind {
            exprs.last().unwrap().ty
        } else {
            chunk_expr.ty
        };

        let expr_value_is_void =
            evaluated_expr_type == types::UNIT || evaluated_expr_type == types::NOTHING;

        compile(
            &self.analyzer.engine,
            &self.analyzer.typing,
            &self.analyzer.resolution.relations,
            &self.analyzer.resolution.engine,
            &self.externals,
            reef,
            SourceId(self.current_page),
            &mut bytes,
            CompilerOptions {
                line_provider: None,
                last_page_storage_var: Some(VAR_EXPR_STORAGE.to_string())
                    .filter(|_| !expr_value_is_void),
            },
        )
        .expect("write failed");

        cli::disassemble::display_bytecode(&bytes);

        self.vm
            .register(&bytes)
            .expect("compilation created invalid bytecode");
        drop(bytes);

        match unsafe { self.vm.run() } {
            Ok(()) => {}
            Err(e) => return Err(e),
        }

        self.current_page += 1;

        if expr_value_is_void {
            return Ok(VmValue::Void);
        }

        let evaluated_value = self.vm.get_exported_var(VAR_EXPR_STORAGE);
        let value = extract_value(evaluated_value, evaluated_expr_type);

        Ok(value)
    }
}

fn extract_value(value: VmValueFFI, value_type: TypeRef) -> VmValue {
    unsafe {
        // SAFETY: the
        match value_type {
            types::EXITCODE | types::BOOL => VmValue::Byte(value.get_as_u8()),
            types::FLOAT => VmValue::Double(value.get_as_double()),
            types::INT => VmValue::Int(value.get_as_i64()),
            types::STRING => VmValue::String(value.get_as_string()),
            types::UNIT => VmValue::Void,
            types::GENERIC_VECTOR => {
                let vec = value
                    .get_as_vec()
                    .into_iter()
                    .map(|val| {
                        //TODO FIXME recursively look for other kind of vector depending on the type
                        extract_value(val, types::STRING)
                    })
                    .collect();
                VmValue::Vec(vec)
            }
            _ => panic!("unknown object"),
        }
    }
}

#[cfg(test)]
mod test {
    use vm::value::VmValue;

    use crate::Runner;

    #[test]
    fn test_runner() {
        let mut runner = Runner::default();
        assert_eq!(
            runner.eval("4.0 * 8.0 / 7.0 + (4.0 - 2.0) * 2.0 - 1.0"),
            VmValue::Double(4.0 * 8.0 / 7.0 + (4.0 - 2.0) * 2.0 - 1.0)
        )
    }

    #[test]
    fn test_runner_assignation() {
        let mut runner = Runner::default();
        runner.eval("var a = 4");
        runner.eval("a += 4");
        assert_eq!(runner.eval("$a"), VmValue::Int(8));
    }
}
