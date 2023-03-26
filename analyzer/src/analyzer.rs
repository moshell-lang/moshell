
use crate::environment::Environment;
use crate::types::{Type};
use crate::Diagnostic;
use ast::function::FunctionParameter;
use ast::value::LiteralValue;
use ast::Expr;
use context::source::Source;
use crate::builtin_types::{float, int, str, unit};

pub struct Analyzer<'a> {
    pub source: Source<'a>,
    pub diagnostics: Vec<Diagnostic>,
}

impl<'a> Analyzer<'a> {
    pub fn new(source: Source<'a>) -> Self {
        Self {
            source,
            diagnostics: Vec::new(),
        }
    }

    pub fn analyze_all(&mut self, expr: &Expr) -> Option<Type> {
        let mut environment = Environment::lang();
        self.analyze(&mut environment, expr)
    }

    fn analyze(&mut self, environment: &mut Environment, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Literal(lit) => {
                let ty = match lit.parsed {
                    LiteralValue::String(_) => str(),
                    LiteralValue::Int(_) => int(),
                    LiteralValue::Float(_) => float(),
                };
                Some(Type::Defined(ty))
            }
            Expr::TemplateString(template) => {
                for part in &template.parts {
                    self.analyze(environment, part);
                }
                Some(Type::Defined(str()))
            }
            Expr::VarDeclaration(decl) => {
                let initializer_type = decl
                    .initializer
                    .as_ref()
                    .map(|init| self.analyze(environment, init))
                    .flatten();
                if let Some(type_hint) = decl.var.ty.as_ref().cloned() {
                    if let Some(initializer_type) = initializer_type.clone() {
                        match environment.types.unify(&initializer_type, &(type_hint.into())) {
                            Ok(_) => {}
                            Err(message) => self.diagnostics.push(Diagnostic { message }),
                        }
                    }
                }

                //environment.define_local(decl.var.name, environment.context().);

                Some(Type::Defined(unit()))
            }
            Expr::VarReference(var) => {
                let hint = environment.lookup_local(var.name);
                if hint.is_none() {
                    self.diagnostics.push(Diagnostic {
                        message: format!("Unknown variable: {}", var.name),
                    });
                }
                hint.map(|l| l.ty.clone())
            }
            Expr::Block(block) => {
                if block.expressions.is_empty() {
                    return Some(Type::Defined(unit()));
                }

                let mut last: Option<_> = None;
                let block_environment = &mut environment.fork();
                for stmt in &block.expressions {
                    last = self.analyze(block_environment, stmt);
                }
                last
            }
            Expr::If(if_expr) => {
                self.analyze(environment, &if_expr.condition)?;
                let expr_type = self.analyze(environment, &if_expr.success_branch)?;
                if let Some(fail) = &if_expr.fail_branch {
                    let fail_type = self.analyze(environment, fail)?;
                    if expr_type != fail_type {
                        self.diagnostics.push(Diagnostic {
                            message: "If branches must return the same type".to_string(),
                        });
                        return None;
                    }
                }
                Some(expr_type)
            }
            Expr::Binary(bin) => {
                let left = self.analyze(environment, &bin.left)?;
                let right = self.analyze(environment, &bin.right)?;
                if let Ok(unified) = environment.types.unify(&left, &right) {
                    Some(unified)
                } else {
                    self.diagnostics.push(Diagnostic {
                        message: "Binary operation must have the same type on both sides"
                            .to_string(),
                    });
                    None
                }
            }
            Expr::Parenthesis(paren) => self.analyze(environment, &paren.expression),
            Expr::Call(call) => {
                let mut last: Option<_> = None;
                for arg in &call.arguments {
                    last = self.analyze(environment, arg);
                }
                last
            }
            Expr::FunctionDeclaration(fun) => {
                let mut nested_environment = environment.fork();
                let mut arg_types = Vec::new();
                for param in &fun.parameters {
                    let param = match param {
                        FunctionParameter::Named(param) => param,
                        _ => todo!("FunctionParameter::Variadic"),
                    };
                    let arg_type = nested_environment.set_local(param.name);
                    arg_types.push(arg_type);
                }
                let ret_type = self.analyze(&mut nested_environment, &fun.body)?;
                /*environment.define_local(
                    fun.name,
                    ClassType {
                        params: vec![],
                        callable: Some(CallableAspect {
                            inputs: arg_types
                                .iter()
                                .map(|arg| Type::Variable(*arg).into())
                                .collect(),
                            output: Type::Variable(ret_type).into(),
                        }),
                    },
                );*/
                todo!("function");
                Some(Type::Defined(unit()))
            }
            Expr::ProgrammaticCall(call) => {
                // if let Some(ty) = environment.context().lookup_definition(call.name) {
                //     if let Some(callable) = ty.callable.as_ref() {
                //         todo!("PFC")
                //     } else {
                //         self.diagnostics.push(Diagnostic {
                //             message: format!("{} is not callable", call.name),
                //         });
                //         None
                //     }
                // } else {
                //     self.diagnostics.push(Diagnostic {
                //         message: format!("Unknown function: {}", call.name),
                //     });
                //     None
                // }
                todo!("PFC")
            }
            _ => Some(Type::Defined(unit())),
        }
    }
}
