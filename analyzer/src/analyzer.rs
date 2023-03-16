use crate::environment::Environment;
use crate::type_scheme::{Type, TypeScheme};
use ast::value::LiteralValue;
use ast::Expr;
use context::source::Source;

pub struct Analyzer<'a> {
    pub source: Source<'a>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, PartialEq)]
pub struct Diagnostic {
    pub message: String,
}

impl<'a> Analyzer<'a> {
    pub fn new(source: Source<'a>) -> Self {
        Self {
            source,
            diagnostics: Vec::new(),
        }
    }

    pub fn analyze_all(&mut self, expr: &Expr) -> Option<TypeScheme> {
        self.analyze(&mut Environment::default(), expr)
    }

    fn analyze(&mut self, environment: &mut Environment, expr: &Expr) -> Option<TypeScheme> {
        match expr {
            Expr::Literal(lit) => Some(
                (match lit.parsed {
                    LiteralValue::String(_) => Type::Any,
                    LiteralValue::Int(_) => Type::Int,
                    LiteralValue::Float(_) => Type::Float,
                })
                .into(),
            ),
            Expr::TemplateString(template) => {
                for part in &template.parts {
                    self.analyze(environment, part);
                }
                Some(Type::Any.into())
            }
            Expr::VarDeclaration(decl) => {
                let actual_type = self.analyze(
                    environment,
                    &decl
                        .initializer
                        .clone()
                        .expect("Not initialized declarations are not supported yet"),
                )?;
                if let Some(type_hint) = decl.var.ty.as_ref() {
                    let ty = Type::try_from(type_hint.name);
                    match ty {
                        Ok(ty) => {
                            if TypeScheme::from(ty) != actual_type {
                                self.diagnostics.push(Diagnostic {
                                    message: format!("Type mismatch: expected {}", type_hint.name),
                                });
                            }
                        }
                        Err(err) => {
                            self.diagnostics.push(Diagnostic { message: err });
                        }
                    }
                }
                environment.add_local(decl.var.name, actual_type);

                // Var declaration doesn't return a value
                Some(Type::Nil.into())
            }
            Expr::VarReference(var) => {
                let hint = environment.lookup(var.name);
                if hint.is_none() {
                    self.diagnostics.push(Diagnostic {
                        message: format!("Unknown variable: {}", var.name),
                    });
                }
                hint
            }
            Expr::Block(block) => {
                if block.expressions.is_empty() {
                    return Some(Type::Nil.into());
                }

                let mut last: Option<TypeScheme> = None;
                environment.begin_scope();
                for stmt in &block.expressions {
                    last = self.analyze(environment, stmt);
                }
                environment.end_scope();
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
                if left != right {
                    self.diagnostics.push(Diagnostic {
                        message: "Binary operation must have the same type on both sides"
                            .to_string(),
                    });
                    return None;
                }
                Some(left)
            }
            Expr::Parenthesis(paren) => self.analyze(environment, &paren.expression),
            Expr::Call(call) => {
                let mut last: Option<TypeScheme> = None;
                for arg in &call.arguments {
                    last = self.analyze(environment, arg);
                }
                last
            }
            _ => Some(Type::Any.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::analyze;
    use crate::analyzer::Diagnostic;
    use crate::type_scheme::Type;
    use context::source::Source;

    #[test]
    fn int_literals_are_const() {
        let source = Source::unknown("1");
        let result = analyze(source).expect("Failed to analyze");
        assert_eq!(result, Type::Int.into(),);
    }

    #[test]
    fn const_plus_const_is_const() {
        let source = Source::unknown("$(( 1 + 2 ))");
        let result = analyze(source).expect("Failed to analyze");
        assert_eq!(result, Type::Int.into(),);
    }

    #[test]
    fn template_of_const() {
        let source = Source::unknown("val n = 9; val str = \"n = $n\"");
        let result = analyze(source).expect("Failed to analyze");
        assert_eq!(result, Type::Nil.into(),);
    }

    #[test]
    fn empty_is_const() {
        let source = Source::unknown("{}");
        let result = analyze(source).expect("Failed to analyze");
        assert_eq!(result, Type::Nil.into(),);
    }

    #[test]
    fn incompatible_types() {
        let source = Source::unknown("$(( 5 + \"5\" ))");
        let result = analyze(source);
        assert_eq!(
            result,
            Err(vec![Diagnostic {
                message: "Binary operation must have the same type on both sides".to_string(),
            }])
        );
    }

    #[test]
    fn out_of_scope() {
        let source = Source::unknown("{ val n = 9 }; echo $n");
        let result = analyze(source);
        assert_eq!(
            result,
            Err(vec![Diagnostic {
                message: "Unknown variable: n".to_string(),
            }])
        );
    }
}
