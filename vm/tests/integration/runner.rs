use std::ffi::OsString;
use std::io;
use std::path::{Path, PathBuf};

use analyzer::hir::ExprKind;
use analyzer::typing::user::{TypeId, UserType};
use analyzer::typing::{registry, user};
use analyzer::{
    analyze_multi, append_source, freeze_exports, Database, FileImporter, Filesystem, Reef,
};
use compiler::{compile_reef, CompilerOptions, CompilerState};
use vm::value::VmValue;
use vm::{VmError, VmValueFFI, VM};

pub struct Runner {
    database: Database,
    compiler_state: CompilerState,
    reef: Reef,
    vm: VM,
}

struct EmptyFilesystem;

impl Filesystem for EmptyFilesystem {
    fn read(&self, _path: &Path) -> io::Result<String> {
        Err(io::Error::new(io::ErrorKind::NotFound, "file not found"))
    }
}

impl Default for Runner {
    fn default() -> Self {
        let fs = FileImporter::new(PathBuf::from("../lib"));
        let mut database = Database::default();
        let mut vm = VM::default();
        let mut reef = Reef::new(OsString::from("std"));
        let errors = analyze_multi(&mut database, &mut reef, &fs, "std");
        assert!(errors.is_empty());

        let mut compiler_state = CompilerState::default();
        let mut bytes = Vec::new();
        compile_reef(
            &database,
            &reef,
            &mut bytes,
            &mut compiler_state,
            CompilerOptions::default(),
        )
        .expect("std did not compile successfully");
        freeze_exports(&mut database, reef);

        vm.register(&bytes).expect("VM std register");
        unsafe {
            vm.run().expect("VM std init");
        }

        Self {
            database,
            compiler_state,
            reef: Reef::new(OsString::from("runner")),
            vm,
        }
    }
}

impl Runner {
    pub fn eval(&mut self, expr: &str) -> Option<VmValue> {
        match self.try_eval(expr) {
            Ok(v) => v,
            Err(VmError::Panic) => panic!("VM did panic"),
            Err(VmError::Internal) => panic!("VM internal error"),
        }
    }

    pub fn try_eval(&mut self, source: &str) -> Result<Option<VmValue>, VmError> {
        let errors = append_source(
            &mut self.database,
            &mut self.reef,
            &EmptyFilesystem,
            PathBuf::from("runner"),
            source,
        );
        assert!(errors.is_empty(), "source code lifts compilation errors");

        let ExprKind::Block(main_block) =
            &self.reef.group_by_content().next().unwrap().main.expr.kind
        else {
            panic!("no main block found");
        };
        let evaluated_expr_type = main_block.last().unwrap().ty;
        let expr_value_is_void =
            evaluated_expr_type == user::UNIT_TYPE || evaluated_expr_type == user::NOTHING_TYPE;

        let mut bytes = Vec::new();
        compile_reef(
            &self.database,
            &self.reef,
            &mut bytes,
            &mut self.compiler_state,
            CompilerOptions {
                last_page_storage_var: Some(VAR_EXPR_STORAGE.to_string()),
                ..CompilerOptions::default()
            },
        )
        .expect("write failed");
        self.vm
            .register(&bytes)
            .expect("compilation created invalid bytecode");
        drop(bytes);
        self.reef.clear_cache();

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

    fn extract_value(&self, value: VmValueFFI, value_type: TypeId) -> Option<VmValue> {
        unsafe {
            match value_type {
                user::BOOL_TYPE | user::EXITCODE_TYPE => Some(VmValue::Byte(value.get_as_u8())),
                user::FLOAT_TYPE => Some(VmValue::Double(value.get_as_double())),
                user::INT_TYPE => Some(VmValue::Int(value.get_as_i64())),
                user::STRING_TYPE => Some(VmValue::String(value.get_as_obj().get_as_string())),
                user::UNIT_TYPE | user::NOTHING_TYPE => Some(VmValue::Void),
                _ => match &self.database.checker.types[value_type] {
                    UserType::Parametrized {
                        schema: registry::OPTION_SCHEMA,
                        params,
                    } => {
                        if value.is_ptr_null() {
                            return None;
                        }
                        let [content_type] = params.as_slice() else {
                            panic!("option instance without content type");
                        };

                        // option can only wrap an object value
                        let value = if content_type.is_obj() {
                            value
                        } else {
                            // unbox it if it was a primitive optional
                            value.get_as_obj().unbox()
                        };

                        self.extract_value(value, *content_type)
                    }
                    UserType::Parametrized {
                        schema: registry::VEC_SCHEMA,
                        params: _,
                    } => {
                        let vec = value
                            .get_as_obj()
                            .get_as_vec()
                            .into_iter()
                            .map(|v| Some(VmValue::deduce(v)))
                            .collect();
                        Some(VmValue::Vec(vec))
                    }
                    UserType::Parametrized { schema, params: _ } => {
                        let structure = &self.database.checker.registry[*schema];
                        // let structure_fields = structure.get_fields();
                        // let structure_layout = &self.current_compiled_reef.layouts[structure_id.0];
                        //
                        // let structure_data = value.get_as_obj().get_as_struct();
                        // let structure_values = structure_fields
                        //     .into_iter()
                        //     .map(|field| {
                        //         let (pos, _) = structure_layout.get_emplacement(field.local_id);
                        //         let field_value = VmValueFFI::ptr(
                        //             *structure_data.as_ptr().add(pos as usize).cast(),
                        //         );
                        //         self.extract_value(field_value, field.ty)
                        //     })
                        //     .collect();
                        //
                        // Some(VmValue::Struct(structure_values))
                        todo!()
                    }
                    _ => panic!("unknown object"),
                },
            }
        }
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
    fn test_runner_variable_ref() {
        let mut runner = Runner::default();
        runner.eval("val x = 9");
        assert_eq!(runner.eval("$x"), Some(VmValue::Int(9)))
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
    fn unit_assignment() {
        let mut runner = Runner::default();
        runner.eval("fun bar() = { var o = {}; o = $o }; var b = bar(); b = {}");
        Some(assert_eq!(runner.eval("$b"), Some(VmValue::Void)));
    }

    #[test]
    fn unit_capture_assignment() {
        let mut runner = Runner::default();
        let res = runner.eval(
            "{
                var foo = {}
                fun bar() = {
                    $foo = {}
                }
            bar()
            }",
        );
        Some(assert_eq!(res, Some(VmValue::Void)));
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
