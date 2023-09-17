use std::path::PathBuf;

use analyzer::importer::{ASTImporter, ImportResult, StaticImporter};
use analyzer::name::Name;
use analyzer::reef::{Externals, Reef};
use analyzer::relations::SourceId;
use analyzer::types::hir::ExprKind;
use analyzer::types::ty::{Type, TypeRef};
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

#[derive(Debug, PartialEq)]
struct GarbageCollection {
    collected_objects: Vec<VmValue>,
}

impl From<Vec<&str>> for GarbageCollection {
    fn from(value: Vec<&str>) -> Self {
        let vec = value
            .into_iter()
            .map(|s| VmValue::String(s.to_string()))
            .collect();

        GarbageCollection {
            collected_objects: vec,
        }
    }
}

impl<'a> Runner<'a> {
    fn eval(&mut self, expr: &'a str) -> VmValue {
        match self.try_eval(expr) {
            Ok(v) => v,
            Err(VmError::Panic) => panic!("VM did panic"),
            Err(VmError::Internal) => panic!("VM internal error"),
        }
    }

    fn try_eval(&mut self, expr: &'a str) -> Result<VmValue, VmError> {
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
        let value = self.extract_value(evaluated_value, evaluated_expr_type);

        Ok(value)
    }

    pub fn gc(&mut self) -> GarbageCollection {
        let result = GarbageCollection {
            collected_objects: self.vm.gc_collect(),
        };
        self.vm.gc();
        result
    }

    fn extract_value(&self, value: VmValueFFI, value_type: TypeRef) -> VmValue {
        unsafe {
            match self.get_type(value_type) {
                Type::Bool | Type::ExitCode => VmValue::Byte(value.get_as_u8()),
                Type::Float => VmValue::Double(value.get_as_double()),
                Type::Int => VmValue::Int(value.get_as_i64()),
                Type::String => VmValue::String(value.get_as_obj().get_as_string()),
                Type::Unit | Type::Nothing => VmValue::Void,
                Type::Instantiated(types::GENERIC_VECTOR, _) => {
                    let vec = value
                        .get_as_obj()
                        .get_as_vec()
                        .into_iter()
                        .map(VmValue::from)
                        .collect();
                    VmValue::Vec(vec)
                }
                _ => panic!("unknown object"),
            }
        }
    }

    fn get_type(&self, tpe: TypeRef) -> &Type {
        let typing = if tpe.reef == self.externals.current {
            &self.analyzer.typing
        } else {
            &self.externals.get_reef(tpe.reef).unwrap().typing
        };
        typing.get_type(tpe.type_id).unwrap()
    }
}

#[cfg(test)]
mod test {
    use vm::value::VmValue;

    use crate::Runner;

    #[test]
    fn test_runner_eval() {
        let mut runner = Runner::default();
        assert_eq!(
            runner.eval("4.0 * 8.0 / 7.0 + (4.0 - 2.0) * 2.0 - 1.0"),
            VmValue::Double(4.0 * 8.0 / 7.0 + (4.0 - 2.0) * 2.0 - 1.0)
        )
    }

    #[test]
    fn test_runner_string() {
        let mut runner = Runner::default();
        assert_eq!(
            runner.eval("'this is a moshell string'"),
            "this is a moshell string".into()
        )
    }

    #[test]
    fn test_runner_vector_eval() {
        let mut runner = Runner::default();
        runner.eval("val vec = 'A B C D E F'.split(' ')");
        runner.eval("$vec.push('G')");
        assert_eq!(
            runner.eval("$vec"),
            vec!["A", "B", "C", "D", "E", "F", "G"].into()
        )
    }

    #[test]
    fn test_runner_assignation() {
        let mut runner = Runner::default();
        runner.eval("var a = 4");
        runner.eval("a += 4");
        runner.eval("var b = 1");
        assert_eq!(runner.eval("$a"), VmValue::Int(8));
    }

    #[test]
    fn test_gc_run() {
        let mut runner = Runner::default();

        runner.eval("var a = 'this string will get ' + 'collected'");
        runner.eval("a = 'string replacement'");
        assert_eq!(runner.gc(), vec!["this string will get collected"].into());
        assert_eq!(runner.eval("$a"), "string replacement".into());
    }
}
