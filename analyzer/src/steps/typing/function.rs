use ast::call::{MethodCall, ProgrammaticCall};
use ast::function::{FunctionDeclaration, FunctionParameter};
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};
use std::fmt;

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation, SourceLocation};
use crate::reef::Externals;
use crate::relations::{Definition, SourceId, SymbolRef};
use crate::steps::typing::coercion::{
    convert_description, convert_expression, convert_many, resolve_type,
};
use crate::steps::typing::exploration::{Exploration, Links};
use crate::types::engine::CodeEntry;
use crate::types::hir::{ExprKind, TypedExpr};
use crate::types::ty::{FunctionType, MethodType, Parameter, Type, TypeRef};
use crate::types::{Typing, ERROR, STRING, UNIT};

/// An identified return during the exploration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Return {
    /// The returned type.
    pub(super) ty: TypeRef,

    /// The segment where the return is located.
    pub(super) segment: SourceSegment,
}

/// Identifies a function that correspond to a call.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct FunctionMatch {
    /// The converted arguments to pass to the function.
    ///
    /// If any conversion is required, it will be done here.
    pub(super) arguments: Vec<TypedExpr>,

    /// The function identifier to call.
    pub(super) definition: Definition,

    /// The function return type.
    pub(super) return_type: TypeRef,
}

/// Gets the returned type of a function.
///
/// This verifies the type annotation if present against all the return types,
/// or try to guess the return type.
pub(super) fn infer_return(
    func: &FunctionDeclaration,
    links: Links,
    typed_func_body: Option<&TypedExpr>,
    diagnostics: &mut Vec<Diagnostic>,
    exploration: &mut Exploration,
) -> TypeRef {
    if let Some(typed_func_body) = typed_func_body {
        let last = get_last_segment(typed_func_body);
        // If the last statement is a return, we don't need re-add it
        if exploration
            .returns
            .last()
            .map_or(true, |ret| ret.segment != last.segment)
            && last.ty.is_something()
            && last.ty.is_ok()
        {
            exploration.returns.push(Return {
                ty: typed_func_body.ty,
                segment: last.segment.clone(),
            });
        }
    }

    let expected_return_type = if let Some(return_type_annotation) = func.return_type.as_ref() {
        // An explicit return type is present, check it against all the return types.
        resolve_type(exploration, links, return_type_annotation, diagnostics)
    } else {
        UNIT
    };

    let mut typed_return_locations: Vec<_> = Vec::new();

    for ret in &exploration.returns {
        if convert_description(exploration, expected_return_type, ret.ty).is_err() {
            typed_return_locations.push(Observation::here(
                links.source,
                ret.segment.clone(),
                if func.return_type.is_some() {
                    format!("Found `{}`", exploration.get_type(ret.ty).unwrap())
                } else {
                    format!("Returning `{}`", exploration.get_type(ret.ty).unwrap())
                },
            ));
        }
    }

    if typed_return_locations.is_empty() {
        return expected_return_type;
    }

    if let Some(return_type_annotation) = func.return_type.as_ref() {
        diagnostics.push(
            Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch")
                .with_observations(typed_return_locations)
                .with_observation(Observation::context(
                    links.source,
                    return_type_annotation.segment(),
                    format!(
                        "Expected `{}` because of return type",
                        exploration.get_type(expected_return_type).unwrap()
                    ),
                )),
        );

        return ERROR;
    }

    let Some(body) = &func.body else {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::CannotInfer,
                "Function declaration needs explicit return type",
            )
            .with_observations(typed_return_locations)
            .with_help("Explicit the function's return type as it's not defined."),
        );

        return ERROR;
    };

    if matches!(body.as_ref(), Expr::Block(_)) {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::CannotInfer,
                "Return type is not inferred for block functions",
            )
            .with_observations(typed_return_locations)
            .with_help("Try adding an explicit return type to the function"),
        );

        return ERROR;
    }
    let segment = func.segment().start..body.segment().start;
    let types: Vec<_> = exploration.returns.iter().map(|ret| ret.ty).collect();
    let unify = convert_many(exploration, types);

    if let Ok(common_type) = unify {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::CannotInfer,
                "Return type inference is not supported yet",
            )
            .with_observation(Observation::context(
                links.source,
                segment,
                "No return type is specified",
            ))
            .with_observations(typed_return_locations)
            .with_help(format!(
                "Add -> {} to the function declaration",
                exploration.get_type(common_type).unwrap()
            )),
        );
    } else {
        diagnostics.push(
            Diagnostic::new(DiagnosticID::CannotInfer, "Failed to infer return type")
                .with_observation(Observation::context(
                    links.source,
                    segment,
                    "This function returns multiple types",
                ))
                .with_observations(typed_return_locations)
                .with_help("Try adding an explicit return type to the function"),
        );
    }
    ERROR
}

/// Checks the type of a call expression.
pub(super) fn type_call(
    call: &ProgrammaticCall,
    exploration: &mut Exploration,
    arguments: Vec<TypedExpr>,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
) -> FunctionMatch {
    let call_symbol_ref = links.env().get_raw_symbol(call.segment()).unwrap();

    let fun_reef = match call_symbol_ref {
        SymbolRef::Local(_) => exploration.externals.current,
        SymbolRef::External(r) => {
            let call_symbol = links.relations[r].state.expect_resolved("unresolved");
            call_symbol.reef
        }
    };

    let type_ref: TypeRef = exploration
        .get_var(
            links.source, /* FIXME need defining env */
            call_symbol_ref,
            links.relations,
        )
        .unwrap()
        .type_ref;

    match exploration.get_type(type_ref).unwrap() {
        &Type::Function(declaration) => {
            let entry: CodeEntry = exploration.get_entry(fun_reef, declaration).unwrap();
            let parameters = entry.parameters().to_owned(); // TODO: avoid clone
            let return_type = entry.return_type();
            if parameters.len() != arguments.len() {
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::TypeMismatch,
                        format!(
                            "This function takes {} {} but {} {} supplied",
                            parameters.len(),
                            pluralize(parameters.len(), "argument", "arguments"),
                            arguments.len(),
                            pluralize(arguments.len(), "was", "were"),
                        ),
                    )
                    .with_observation(Observation::here(
                        links.source,
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
                for (param, arg) in parameters.iter().cloned().zip(arguments) {
                    casted_arguments.push(
                        match convert_expression(
                            arg,
                            param.ty,
                            exploration,
                            links.source,
                            diagnostics,
                        ) {
                            Ok(arg) => arg,
                            Err(arg) => {
                                diagnostics.push(diagnose_arg_mismatch(
                                    exploration,
                                    links.source,
                                    &param,
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
                    "Cannot invoke non function type",
                )
                .with_observation(Observation::here(
                    links.source,
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
    source: SourceId,
) -> Option<&'a MethodType> {
    if callee.ty.is_err() {
        return None;
    }

    // Directly callable types just have a single method called `apply`
    let method_name = method_call.name.unwrap_or("apply");
    let type_methods = exploration.get_methods(callee.ty, method_name);
    if type_methods.is_none() {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::UnknownMethod,
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
            )
            .with_observation((source, method_call.segment.clone()).into()),
        );
        return None;
    }

    let methods = type_methods.unwrap(); // We just checked for None
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
                    format!(
                        "This method takes {} {} but {} {} supplied",
                        method.parameters.len(),
                        pluralize(method.parameters.len(), "argument", "arguments"),
                        arguments.len(),
                        pluralize(arguments.len(), "was", "were")
                    ),
                )
                .with_observation(Observation::here(
                    source,
                    method_call.segment(),
                    "Method is called here",
                ))
                .with_help(format!(
                    "The method signature is `{}::{}`",
                    exploration.get_type(callee.ty).unwrap(),
                    Signature::new(
                        &exploration.typing,
                        exploration.externals,
                        method_name,
                        method
                    )
                )),
            );
        } else {
            for (param, arg) in method.parameters.iter().zip(arguments.iter()) {
                if convert_description(exploration, param.ty, arg.ty).is_err() {
                    let diagnostic = diagnose_arg_mismatch(exploration, source, param, arg)
                        .with_observation(Observation::here(
                            source,
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
                format!(
                    "No matching method found for `{method_name}::{}`",
                    exploration.get_type(callee.ty).unwrap()
                ),
            )
            .with_observation(Observation::here(
                source,
                method_call.segment(),
                "Method is called here",
            )),
        );
    }
    None
}

/// Generates a type mismatch between a parameter and an argument.
fn diagnose_arg_mismatch(
    exploration: &Exploration,
    source: SourceId,
    param: &Parameter,
    arg: &TypedExpr,
) -> Diagnostic {
    let diagnostic = Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch").with_observation(
        Observation::here(
            source,
            arg.segment.clone(),
            format!(
                "Expected `{}`, found `{}`",
                exploration.get_type(param.ty).unwrap(),
                exploration.get_type(arg.ty).unwrap()
            ),
        ),
    );
    if let Some(location) = &param.location {
        diagnostic.with_observation(Observation::context(
            location.source,
            location.segment.clone(),
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
pub(super) fn type_parameter(
    exploration: &mut Exploration,
    param: &FunctionParameter,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
) -> Parameter {
    match param {
        FunctionParameter::Named(named) => {
            let type_id = named.ty.as_ref().map_or(STRING, |ty| {
                resolve_type(exploration, links, ty, diagnostics)
            });
            Parameter {
                location: Some(SourceLocation::new(links.source, named.segment.clone())),
                ty: type_id,
            }
        }
        FunctionParameter::Variadic(_) => todo!("Arrays are not supported yet"),
    }
}

fn get_last_segment(expr: &TypedExpr) -> &TypedExpr {
    match &expr.kind {
        ExprKind::Block(expressions) => expressions.last().map_or(expr, get_last_segment),
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
    externals: &'a Externals<'a>,
    name: &'a str,
    function: &'a FunctionType,
}

impl<'a> Signature<'a> {
    /// Creates a new signature.
    fn new(
        typing: &'a Typing,
        externals: &'a Externals,
        name: &'a str,
        function: &'a FunctionType,
    ) -> Self {
        Self {
            typing,
            externals,
            name,
            function,
        }
    }

    fn get_type(&self, id: TypeRef) -> &Type {
        if id.reef == self.externals.current {
            self.typing.get_type(id.type_id).unwrap_or(&Type::Error)
        } else {
            self.externals
                .get_reef(id.reef)
                .unwrap()
                .typing
                .get_type(id.type_id)
                .unwrap_or(&Type::Error)
        }
    }
}

impl fmt::Display for Signature<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.name)?;
        if let Some((first, parameters)) = self.function.parameters.split_first() {
            write!(f, "{}", self.get_type(first.ty))?;
            for param in parameters {
                write!(f, ", {}", self.get_type(param.ty))?;
            }
        }
        if self.function.return_type.is_nothing() {
            write!(f, ")")
        } else {
            write!(f, ") -> {}", self.get_type(self.function.return_type))
        }
    }
}
