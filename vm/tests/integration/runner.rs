use std::path::PathBuf;

use analyzer::importer::{ASTImporter, ImportResult, StaticImporter};
use analyzer::name::Name;
use analyzer::reef::{Externals, Reef};
use analyzer::relations::SourceId;
use analyzer::types::ty::{Type, TypeRef};
use analyzer::{analyze, types, Analyzer, Inject};
use cli::pipeline::FileImporter;
use compiler::externals::{CompiledReef, CompilerExternals};
use compiler::{compile_reef, CompilerOptions};
use context::source::Source;
use parser::parse_trusted;
use vm::value::VmValue;
use vm::{VmError, VmValueFFI, VM};

pub struct Runner<'a> {
    externals: Externals<'a>,
    compiler_externals: CompilerExternals,
    current_compiled_reef: CompiledReef,
    vm: VM,
    analyzer: Analyzer<'a>,
    current_page: Option<SourceId>,
}

impl Default for Runner<'_> {
    fn default() -> Self {
        let mut externals = Externals::default();
        let compiler_externals = CompilerExternals::default();
        let mut std_importer = FileImporter::new(PathBuf::from("../lib"));
        let mut vm = VM::default();

        let std_name = Name::new("std");
        let analyzer = analyze(std_name.clone(), &mut std_importer, &externals);
        let mut buff = Vec::new();

        compile_reef(
            &analyzer.engine,
            &analyzer.resolution.relations,
            &analyzer.typing,
            &analyzer.resolution.engine,
            &externals,
            &compiler_externals,
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
            externals,
            vm,
            compiler_externals,
            current_compiled_reef: CompiledReef::default(),
            analyzer: Analyzer::default(),
            current_page: None,
        }
    }
}

impl<'a> Runner<'a> {
    pub fn eval(&mut self, expr: &'a str) -> Option<VmValue> {
        match self.try_eval(expr) {
            Ok(v) => v,
            Err(VmError::Panic) => panic!("VM did panic"),
            Err(VmError::Internal) => panic!("VM internal error"),
        }
    }

    pub fn try_eval(&mut self, expr: &'a str) -> Result<Option<VmValue>, VmError> {
        let name = Name::new("runner");
        let src = Source::unknown(expr);
        let mut importer = StaticImporter::new([(name.clone(), src)], parse_trusted);
        let ImportResult::Success(imported) = importer.import(&name) else {
            unreachable!()
        };

        let inject = Inject {
            name: name.clone(),
            imported,
            attached: self.current_page,
        };

        let mut analysis = self.analyzer.inject(inject, &mut importer, &self.externals);
        let page = analysis.attributed_id();
        self.current_page = Some(page);
        let diagnostics = analysis.take_diagnostics();

        let reef = self.externals.current;

        if !diagnostics.is_empty() {
            panic!("input had analysis errors: \n{diagnostics:?}")
        }
        let mut bytes = Vec::new();

        let chunk = self.analyzer.engine.get_user(page).unwrap();
        let chunk_expr = &chunk.expression.as_ref().unwrap();

        let evaluated_expr_type = chunk_expr.ty;

        let expr_value_is_void =
            evaluated_expr_type == types::UNIT || evaluated_expr_type == types::NOTHING;

        self.current_compiled_reef = compile_reef(
            &self.analyzer.engine,
            &self.analyzer.resolution.relations,
            &self.analyzer.typing,
            &self.analyzer.resolution.engine,
            &self.externals,
            &self.compiler_externals,
            reef,
            page,
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

        if expr_value_is_void {
            return Ok(Some(VmValue::Void));
        }

        let evaluated_value = self.vm.get_exported_var(VAR_EXPR_STORAGE);
        let value = self.extract_value(evaluated_value, evaluated_expr_type);

        Ok(value)
    }

    pub fn gc(&mut self) -> GarbageCollection {
        let result = GarbageCollection {
            collected_objects: self.vm.gc.collect(),
        };
        self.vm.gc.run();
        result
    }

    fn extract_value(&self, value: VmValueFFI, value_type: TypeRef) -> Option<VmValue> {
        unsafe {
            match value_type {
                types::BOOL | types::EXITCODE => Some(VmValue::Byte(value.get_as_u8())),
                types::FLOAT => Some(VmValue::Double(value.get_as_double())),
                types::INT => Some(VmValue::Int(value.get_as_i64())),
                types::STRING => Some(VmValue::String(value.get_as_obj().get_as_string())),
                types::UNIT | types::NOTHING => Some(VmValue::Void),
                _ => match self.get_type(value_type) {
                    Type::Instantiated(types::GENERIC_OPTION, param) => {
                        if value.is_ptr_null() {
                            return None;
                        }
                        let content_type =
                            *param.first().expect("option instance without content type");

                        // option can only wrap an object value
                        let value = if content_type.is_obj() {
                            value
                        } else {
                            // unbox it if it was a primitive optional
                            value.get_as_obj().unbox()
                        };

                        self.extract_value(value, content_type)
                    }
                    Type::Instantiated(types::GENERIC_VECTOR, _) => {
                        let vec = value
                            .get_as_obj()
                            .get_as_vec()
                            .into_iter()
                            .map(|v| Some(VmValue::deduce(v)))
                            .collect();
                        Some(VmValue::Vec(vec))
                    }
                    Type::Structure(_, structure_id) => {
                        let structure = self.analyzer.engine.get_structure(*structure_id).unwrap();
                        let structure_fields = structure.get_fields();
                        let structure_layout = &self.current_compiled_reef.layouts[structure_id.0];

                        let structure_data = value.get_as_obj().get_as_struct();
                        let structure_values = structure_fields
                            .into_iter()
                            .map(|field| {
                                let (pos, _) = structure_layout.get_emplacement(field.local_id);
                                let field_value = VmValueFFI::ptr(
                                    *structure_data.as_ptr().add(pos as usize).cast(),
                                );
                                self.extract_value(field_value, field.ty)
                            })
                            .collect();

                        Some(VmValue::Struct(structure_values))
                    }
                    _ => panic!("unknown object"),
                },
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

// use an invalid name in moshell's language specs
const VAR_EXPR_STORAGE: &str = "7 ! ^";

#[derive(Debug, PartialEq)]
pub struct GarbageCollection {
    collected_objects: Vec<VmValue>,
}

impl GarbageCollection {
    fn extract(&mut self, v: impl Into<VmValue>) -> bool {
        let v = v.into();
        let Some(idx) = self.collected_objects.iter().position(|l| *l == v) else {
            return false;
        };
        self.collected_objects.remove(idx);
        true
    }
}

impl From<Vec<&str>> for GarbageCollection {
    fn from(values: Vec<&str>) -> Self {
        let vec = values
            .into_iter()
            .map(|s| VmValue::String(s.to_string()))
            .collect();

        GarbageCollection {
            collected_objects: vec,
        }
    }
}

impl From<Vec<VmValue>> for GarbageCollection {
    fn from(values: Vec<VmValue>) -> Self {
        GarbageCollection {
            collected_objects: values,
        }
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use crate::runner::Runner;
    use vm::value::VmValue;

    #[test]
    fn test_runner_eval() {
        let mut runner = Runner::default();
        assert_eq!(
            runner.eval("4.0 * 8.0 / 7.0 + (4.0 - 2.0) * 2.0 - 1.0"),
            Some(VmValue::Double(4.0 * 8.0 / 7.0 + (4.0 - 2.0) * 2.0 - 1.0))
        )
    }

    #[test]
    fn test_runner_string() {
        let mut runner = Runner::default();
        assert_eq!(
            runner.eval("'this is a moshell string'"),
            Some("this is a moshell string".into())
        )
    }

    #[test]
    fn test_runner_vector_eval() {
        let mut runner = Runner::default();
        runner.eval("val vec = 'A B C D E F'.split(' ')");
        runner.eval("$vec.push('G')");
        assert_eq!(
            runner.eval("$vec"),
            Some(vec!["A", "B", "C", "D", "E", "F", "G"].into())
        )
    }

    #[test]
    fn test_runner_assignation() {
        let mut runner = Runner::default();
        runner.eval("var a = 4");
        runner.eval("a += 4");
        runner.eval("var b = 1");
        Some(assert_eq!(runner.eval("$a"), Some(VmValue::Int(8))));
    }

    #[test]
    fn test_gc_run() {
        let mut runner = Runner::default();

        runner.eval("var a = ('this string will get collected').split(' ')");
        runner.eval("a = ''.split(' ')");

        let mut collect = runner.gc();
        // assert for the vector
        assert!(collect.extract(vec!["this", "string", "will", "get", "collected"]));
        // assert for the string elements
        assert_eq!(
            collect,
            vec!["collected", "get", "will", "string", "this"].into()
        );

        assert_eq!(runner.eval("$a"), Some(vec![""].into()));
    }
}
