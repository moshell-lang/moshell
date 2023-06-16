use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::relations::{Relations, SourceId, Symbol};
use crate::steps::typing::coercion::unify_and_map;
use crate::steps::typing::exploration::Exploration;
use crate::steps::typing::TypingState;
use crate::types::ctx::TypeContext;
use crate::types::hir::{ExprKind, TypeId, TypedExpr};
use crate::types::ty::{Definition, FunctionType, MethodType, Parameter, Type};
use crate::types::{Typing, ERROR, NOTHING, STRING};
use ast::call::{MethodCall, ProgrammaticCall};
use ast::function::{FunctionDeclaration, FunctionParameter};
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};
use std::fmt;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct Return {
    pub(super) ty: TypeId,
    pub(super) segment: SourceSegment,
}

pub(super) struct FunctionMatch {
    pub(super) arguments: Vec<TypedExpr>,
    pub(super) definition: Definition,
    pub(super) return_type: TypeId,
}

/// Gets the returned type of a function.
///
/// This verifies the type annotation if present against all the return types,
/// or try to guess the return type.
pub(super) fn infer_return(
    func: &FunctionDeclaration,
    typed_func: &TypedExpr,
    diagnostics: &mut Vec<Diagnostic>,
    exploration: &mut Exploration,
    state: TypingState,
) -> TypeId {
    let last = get_last_segment(typed_func);
    // If the last statement is a return, we don't need re-add it
    if exploration
        .returns
        .last()
        .map(|ret| ret.segment != last.segment)
        .unwrap_or(true)
        && (last.ty.is_something() || !exploration.returns.is_empty() && func.return_type.is_none())
    {
        exploration.returns.push(Return {
            ty: typed_func.ty,
            segment: last.segment.clone(),
        });
    }

    if let Some(return_type_annotation) = func.return_type.as_ref() {
        // An explicit return type is present, check it against all the return types.
        let type_annotation = exploration
            .ctx
            .resolve(return_type_annotation)
            .unwrap_or(ERROR);
        if type_annotation == ERROR {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::UnknownType,
                    state.source,
                    "Unknown type annotation",
                )
                .with_observation(Observation::with_help(
                    return_type_annotation.segment(),
                    "Not found in scope",
                )),
            );
        } else {
            for ret in &exploration.returns {
                if exploration.typing.unify(type_annotation, ret.ty).is_err() {
                    diagnostics.push(
                        Diagnostic::new(DiagnosticID::TypeMismatch, state.source, "Type mismatch")
                            .with_observation(Observation::with_help(
                                ret.segment.clone(),
                                format!("Found `{}`", exploration.get_type(ret.ty).unwrap()),
                            ))
                            .with_observation(Observation::with_help(
                                return_type_annotation.segment(),
                                format!(
                                    "Expected `{}` because of return type",
                                    exploration.get_type(type_annotation).unwrap()
                                ),
                            )),
                    );
                }
            }
        }
        type_annotation
    } else if !matches!(func.body.as_ref(), Expr::Block(_)) {
        // We may want to infer, or leave it as a empty return type
        match exploration
            .typing
            .unify_many(exploration.returns.iter().map(|ret| ret.ty))
        {
            Ok(ty) if ty.is_nothing() => ty,
            Ok(ty) => {
                let segment = func.segment().start..func.body.segment().start;
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::CannotInfer,
                        state.source,
                        "Return type inference is not supported yet",
                    )
                    .with_observation(Observation::with_help(
                        segment,
                        "No return type is specified",
                    ))
                    .with_help("Add -> Float to the function declaration"),
                );
                ty
            }
            Err(_) => {
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::CannotInfer,
                        state.source,
                        "Failed to infer return type",
                    )
                    .with_observation(Observation::with_help(
                        func.segment(),
                        "This function returns multiple types".to_string(),
                    ))
                    .with_help("Try adding an explicit return type to the function"),
                );
                ERROR
            }
        }
    } else {
        // Explain if there is any return that this function will not be inferred
        let mut observations = Vec::new();
        for ret in &exploration.returns {
            observations.push(Observation::with_help(
                ret.segment.clone(),
                format!("Returning `{}`", exploration.get_type(ret.ty).unwrap()),
            ));
        }
        if !observations.is_empty() {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::CannotInfer,
                    state.source,
                    "Return type is not inferred for block functions",
                )
                .with_observations(observations)
                .with_help("Try adding an explicit return type to the function"),
            );
        }
        NOTHING
    }
}

/// Checks the type of a call expression.
pub(super) fn type_call(
    call: &ProgrammaticCall,
    arguments: Vec<TypedExpr>,
    symbol: Symbol,
    diagnostics: &mut Vec<Diagnostic>,
    exploration: &mut Exploration,
    relations: &Relations,
    state: TypingState,
) -> FunctionMatch {
    let type_id = exploration
        .ctx
        .get(relations, state.source, symbol)
        .unwrap()
        .type_id;
    match exploration.get_type(type_id).unwrap() {
        Type::Function(declaration) => {
            let declaration = *declaration;
            let entry = exploration.engine.get(declaration).unwrap();
            let parameters = entry.parameters();
            let return_type = entry.return_type();
            if parameters.len() != arguments.len() {
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::TypeMismatch,
                        state.source,
                        format!(
                            "This function takes {} {} but {} {} supplied",
                            parameters.len(),
                            pluralize(parameters.len(), "argument", "arguments"),
                            arguments.len(),
                            pluralize(arguments.len(), "was", "were"),
                        ),
                    )
                    .with_observation(Observation::with_help(
                        call.segment.clone(),
                        "Function is called here",
                    )),
                );
                FunctionMatch {
                    arguments,
                    definition: Definition::error(),
                    return_type,
                }
            } else {
                let mut casted_arguments = Vec::with_capacity(parameters.len());
                for (param, arg) in parameters.iter().zip(arguments.into_iter()) {
                    casted_arguments.push(
                        match unify_and_map(
                            arg,
                            param.ty,
                            &mut exploration.typing,
                            &exploration.engine,
                            state,
                            diagnostics,
                        ) {
                            Ok(arg) => arg,
                            Err(arg) => {
                                diagnostics.push(diagnose_arg_mismatch(
                                    &exploration.typing,
                                    state.source,
                                    param,
                                    &arg,
                                ));
                                arg
                            }
                        },
                    );
                }
                FunctionMatch {
                    arguments: casted_arguments,
                    definition: declaration,
                    return_type,
                }
            }
        }
        ty => {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    state.source,
                    "Cannot invoke non function type",
                )
                .with_observation(Observation::with_help(
                    call.segment(),
                    format!("Call expression requires function, found `{ty}`"),
                )),
            );
            FunctionMatch {
                arguments,
                definition: Definition::error(),
                return_type: ERROR,
            }
        }
    }
}

/// Checks the type of a method expression.
pub(super) fn find_operand_implementation<'a>(
    methods: &'a [MethodType],
    right: &TypedExpr,
) -> Option<&'a MethodType> {
    for method in methods {
        if method.parameters.len() != 1 {
            continue;
        }
        if let Some(ty) = method.parameters.first() {
            if ty.ty == right.ty {
                return Some(method);
            }
        }
    }
    None
}

/// Checks the type of a method expression.
pub(super) fn type_method<'a>(
    method_call: &MethodCall,
    callee: &TypedExpr,
    arguments: &[TypedExpr],
    diagnostics: &mut Vec<Diagnostic>,
    exploration: &'a mut Exploration,
    state: TypingState,
) -> Option<&'a MethodType> {
    // Directly callable types just have a single method called `apply`
    let method_name = method_call.name.unwrap_or("apply");
    let methods = exploration.engine.get_methods(callee.ty, method_name);
    if methods.is_none() {
        diagnostics.push(Diagnostic::new(
            DiagnosticID::UnknownMethod,
            state.source,
            if method_call.name.is_some() {
                format!(
                    "No method named `{method_name}` found for type `{}`",
                    exploration.get_type(callee.ty).unwrap()
                )
            } else {
                format!(
                    "Type `{}` is not directly callable",
                    exploration.get_type(callee.ty).unwrap()
                )
            },
        ));
        return None;
    }

    let methods = methods.unwrap(); // We just checked for None
    let method = find_exact_method(methods, arguments);
    if let Some(method) = method {
        // We have an exact match
        return Some(method);
    }

    if methods.len() == 1 {
        // If there is only one method, we can give a more specific error by adding
        // an observation for each invalid type
        let method = methods.first().unwrap();
        if method.parameters.len() != arguments.len() {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    state.source,
                    format!(
                        "This method takes {} {} but {} {} supplied",
                        method.parameters.len(),
                        pluralize(method.parameters.len(), "argument", "arguments"),
                        arguments.len(),
                        pluralize(arguments.len(), "was", "were")
                    ),
                )
                .with_observation(Observation::with_help(
                    method_call.segment(),
                    "Method is called here",
                ))
                .with_help(format!(
                    "The method signature is `{}::{}`",
                    exploration.get_type(callee.ty).unwrap(),
                    Signature::new(&exploration.typing, method_name, method)
                )),
            );
        } else {
            for (param, arg) in method.parameters.iter().zip(arguments.iter()) {
                if exploration.typing.unify(param.ty, arg.ty).is_err() {
                    let diagnostic =
                        diagnose_arg_mismatch(&exploration.typing, state.source, param, arg)
                            .with_observation(Observation::with_help(
                                method_call.segment(),
                                "Arguments to this method are incorrect",
                            ));
                    diagnostics.push(diagnostic);
                }
            }
        }
    } else {
        // If there are multiple methods, list them all
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::UnknownMethod,
                state.source,
                format!(
                    "No matching method found for `{method_name}::{}`",
                    exploration.get_type(callee.ty).unwrap()
                ),
            )
            .with_observation(Observation::with_help(
                method_call.segment(),
                "Method is called here",
            )),
        );
    }
    None
}

fn diagnose_arg_mismatch(
    typing: &Typing,
    source: SourceId,
    param: &Parameter,
    arg: &TypedExpr,
) -> Diagnostic {
    let diagnostic = Diagnostic::new(DiagnosticID::TypeMismatch, source, "Type mismatch")
        .with_observation(Observation::with_help(
            arg.segment.clone(),
            format!(
                "Expected `{}`, found `{}`",
                typing.get_type(param.ty).unwrap(),
                typing.get_type(arg.ty).unwrap()
            ),
        ));
    if let Some(decl) = &param.segment {
        diagnostic.with_observation(Observation::with_help(
            decl.clone(),
            "Parameter is declared here",
        ))
    } else {
        diagnostic
    }
}

/// Find a matching method for the given arguments.
fn find_exact_method<'a>(methods: &'a [MethodType], args: &[TypedExpr]) -> Option<&'a MethodType> {
    for method in methods {
        if method.parameters.len() != args.len() {
            continue;
        }
        let mut matches = true;
        for (param, arg) in method.parameters.iter().zip(args.iter()) {
            if param.ty != arg.ty {
                matches = false;
                break;
            }
        }
        if matches {
            return Some(method);
        }
    }
    None
}

/// Type check a single function parameter.
pub(crate) fn type_parameter(ctx: &TypeContext, param: &FunctionParameter) -> Parameter {
    match param {
        FunctionParameter::Named(named) => {
            let type_id = named
                .ty
                .as_ref()
                .map(|ty| ctx.resolve(ty).unwrap_or(ERROR))
                .unwrap_or(STRING);
            Parameter {
                segment: Some(named.segment.clone()),
                ty: type_id,
            }
        }
        FunctionParameter::Variadic(_) => todo!("Arrays are not supported yet"),
    }
}

fn get_last_segment(expr: &TypedExpr) -> &TypedExpr {
    match &expr.kind {
        ExprKind::Block(expressions) => expressions.last().map(get_last_segment).unwrap_or(expr),
        _ => expr,
    }
}

fn pluralize<'a>(count: usize, singular: &'a str, plural: &'a str) -> &'a str {
    if count == 1 {
        singular
    } else {
        plural
    }
}

/// A formatted signature of a function.
struct Signature<'a> {
    typing: &'a Typing,
    name: &'a str,
    function: &'a FunctionType,
}

impl<'a> Signature<'a> {
    /// Creates a new signature.
    fn new(typing: &'a Typing, name: &'a str, function: &'a FunctionType) -> Self {
        Self {
            typing,
            name,
            function,
        }
    }
}

impl Display for Signature<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.name)?;
        if let Some((first, parameters)) = self.function.parameters.split_first() {
            write!(
                f,
                "{}",
                self.typing.get_type(first.ty).unwrap_or(&Type::Error)
            )?;
            for param in parameters {
                write!(
                    f,
                    ", {}",
                    self.typing.get_type(param.ty).unwrap_or(&Type::Error)
                )?;
            }
        }
        if self.function.return_type.is_nothing() {
            write!(f, ")")
        } else {
            write!(
                f,
                ") -> {}",
                self.typing
                    .get_type(self.function.return_type)
                    .unwrap_or(&Type::Error)
            )
        }
    }
}
