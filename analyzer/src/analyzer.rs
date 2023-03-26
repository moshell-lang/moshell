use crate::environment::Environment;
use crate::types::{Type, TypeScheme, Variable};
use crate::Diagnostic;
use ast::value::LiteralValue;
use ast::Expr;
use context::source::Source;

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

    pub fn analyze_all(&mut self, expr: &Expr) -> Option<TypeScheme> {
        let mut environment = Environment::default();
        environment.context_mut().fill_with_builtins();
        let ret = self.analyze(&mut environment, expr);
        ret.map(|ty| environment.context().extract(ty)).flatten()
    }

    fn analyze(&mut self, environment: &mut Environment, expr: &Expr) -> Option<Variable> {
        match expr {
            Expr::Literal(lit) => {
                let ty = match lit.parsed {
                    LiteralValue::String(_) => environment.emit_string(),
                    LiteralValue::Int(_) => environment.emit_int(),
                    LiteralValue::Float(_) => environment.emit_float(),
                };
                Some(ty)
            }
            Expr::TemplateString(template) => {
                for part in &template.parts {
                    self.analyze(environment, part);
                }
                Some(environment.emit_string())
            }
            Expr::VarDeclaration(decl) => {
                let initializer_type = decl
                    .initializer
                    .as_ref()
                    .map(|init| self.analyze(environment, init))
                    .flatten();
                if let Some(type_hint) = decl.var.ty.as_ref() {
                    if let Ok(actual_type) = environment.context().resolve(&(type_hint.into())) {
                        if let Some(initializer_type) = initializer_type {
                            match environment
                                .context_mut()
                                .unify(initializer_type, actual_type)
                            {
                                Ok(_) => {}
                                Err(message) => self.diagnostics.push(Diagnostic { message }),
                            }
                        }
                    } else {
                        self.diagnostics.push(Diagnostic {
                            message: format!("Unknown type: {}", type_hint),
                        });
                    }
                }
                let var = environment.add_local(decl.var.name, initializer_type.is_some());
                if let Some(initializer_type) = initializer_type {
                    environment
                        .context_mut()
                        .extend(var, Type::Variable(initializer_type));
                }
                Some(environment.emit_nil().into())
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
                    return Some(environment.emit_nil().into());
                }

                let mut last: Option<Variable> = None;
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
                if let Ok(unified) = environment.context_mut().unify(left, right) {
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
                let mut last: Option<Variable> = None;
                for arg in &call.arguments {
                    last = self.analyze(environment, arg);
                }
                last
            }
            _ => Some(environment.emit_nil().into()),
        }
    }
}
