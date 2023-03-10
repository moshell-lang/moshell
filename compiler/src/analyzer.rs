use crate::ValHint;
use crate::ValType;
use context::source::Source;
use parser::ast::value::LiteralValue;
use parser::ast::Expr;
use std::collections::HashMap;

pub struct Analyzer<'a> {
    pub source: Source<'a>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, PartialEq)]
pub struct Diagnostic {
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Default)]
struct Scope {
    vars: HashMap<String, ValHint>,
}

impl<'a> Analyzer<'a> {
    pub fn new(source: Source<'a>) -> Self {
        Self {
            source,
            diagnostics: Vec::new(),
        }
    }

    pub fn analyze_all(&mut self, expr: &Expr) -> Option<ValHint> {
        self.analyze(&mut Scope::default(), expr)
    }

    fn analyze(&mut self, scope: &mut Scope, expr: &Expr) -> Option<ValHint> {
        match expr {
            Expr::Literal(lit) => Some(ValHint {
                val_type: match lit.parsed {
                    LiteralValue::String(_) => ValType::Any,
                    LiteralValue::Int(_) => ValType::Int,
                    LiteralValue::Float(_) => ValType::Float,
                },
                is_const: true,
            }),
            Expr::TemplateString(template) => Some(ValHint {
                val_type: ValType::Any,
                is_const: template.parts.iter().all(|part| {
                    self.analyze(scope, part)
                        .map_or(false, |hint| hint.is_const)
                }),
            }),
            Expr::VarDeclaration(decl) => {
                let actual_type = self.analyze(
                    scope,
                    &decl
                        .initializer
                        .clone()
                        .expect("Not initialized declarations are not supported yet"),
                )?;
                if let Some(type_hint_str) = decl.var.ty {
                    let type_hint = ValType::try_from(type_hint_str);
                    match type_hint {
                        Ok(type_hint) => {
                            if type_hint != actual_type.val_type {
                                self.diagnostics.push(Diagnostic {
                                    message: format!("Type mismatch: expected {type_hint_str}"),
                                });
                            }
                        }
                        Err(err) => {
                            self.diagnostics.push(Diagnostic { message: err });
                        }
                    }
                }
                scope.vars.insert(decl.var.name.to_owned(), actual_type);

                // Var declaration doesn't return a value
                Some(ValHint {
                    val_type: ValType::Nil,
                    is_const: actual_type.is_const,
                })
            }
            Expr::VarReference(var) => {
                let hint = scope.vars.get(var.name).cloned();
                if hint.is_none() {
                    self.diagnostics.push(Diagnostic {
                        message: format!("Unknown variable: {}", var.name),
                    });
                }
                hint
            }
            Expr::Block(block) => {
                if block.expressions.is_empty() {
                    return Some(ValHint::default());
                }

                let mut is_const = true;
                let mut last: Option<ValHint> = None;
                let mut scope = scope.clone();
                for stmt in &block.expressions {
                    last = self.analyze(&mut scope, stmt);
                    is_const &= last.map_or(false, |hint| hint.is_const);
                }
                last.map(|mut hint| {
                    hint.is_const = is_const;
                    hint
                })
            }
            Expr::If(if_expr) => {
                let condition = self.analyze(scope, &if_expr.condition)?;
                let expr_type = self.analyze(scope, &if_expr.success_branch)?;
                let mut is_body_const = expr_type.is_const;
                if let Some(fail) = &if_expr.fail_branch {
                    let fail_type = self.analyze(scope, fail)?;
                    if expr_type.val_type != fail_type.val_type {
                        self.diagnostics.push(Diagnostic {
                            message: "If branches must return the same type".to_string(),
                        });
                        return None;
                    }
                    is_body_const &= fail_type.is_const;
                }
                Some(ValHint {
                    val_type: expr_type.val_type,
                    is_const: condition.is_const || is_body_const,
                })
            }
            Expr::Binary(bin) => {
                let left = self.analyze(scope, &bin.left)?;
                let right = self.analyze(scope, &bin.right)?;
                if left.val_type != right.val_type {
                    self.diagnostics.push(Diagnostic {
                        message: "Binary operation must have the same type on both sides"
                            .to_string(),
                    });
                    return None;
                }
                Some(ValHint {
                    val_type: left.val_type,
                    is_const: left.is_const && right.is_const,
                })
            }
            Expr::Parenthesis(paren) => self.analyze(scope, &paren.expression),
            Expr::Call(call) => {
                let mut is_const = true;
                let mut last: Option<ValHint> = None;
                for arg in &call.arguments {
                    last = self.analyze(scope, arg);
                    is_const &= last.map_or(false, |hint| hint.is_const);
                }
                last.map(|mut hint| {
                    hint.is_const = is_const;
                    hint
                })
            }
            _ => Some(ValHint {
                val_type: ValType::Nil,
                is_const: false,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::Diagnostic;
    use crate::{analyze, ValHint, ValType};
    use context::source::Source;

    #[test]
    fn int_literals_are_const() {
        let source = Source::unknown("1");
        let result = analyze(source).expect("Failed to analyze");
        assert_eq!(
            result,
            ValHint {
                val_type: ValType::Int,
                is_const: true,
            }
        );
    }

    #[test]
    fn const_plus_const_is_const() {
        let source = Source::unknown("$(( 1 + 2 ))");
        let result = analyze(source).expect("Failed to analyze");
        assert_eq!(
            result,
            ValHint {
                val_type: ValType::Int,
                is_const: true,
            }
        );
    }

    #[test]
    fn template_of_const() {
        let source = Source::unknown("val n = 9; val str = \"n = $n\"");
        let result = analyze(source).expect("Failed to analyze");
        assert_eq!(
            result,
            ValHint {
                val_type: ValType::Nil,
                is_const: true,
            }
        );
    }

    #[test]
    fn empty_is_const() {
        let source = Source::unknown("{}");
        let result = analyze(source).expect("Failed to analyze");
        assert_eq!(
            result,
            ValHint {
                val_type: ValType::Nil,
                is_const: true,
            }
        );
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
