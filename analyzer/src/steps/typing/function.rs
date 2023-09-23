use std::collections::HashMap;
use std::fmt;

use ast::call::{MethodCall, ProgrammaticCall};
use ast::function::{FunctionDeclaration, FunctionParameter};
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation, SourceLocation};
use crate::reef::ReefId;
use crate::relations::{Definition, LocalId, SourceId, SymbolRef};
use crate::steps::typing::bounds::TypesBounds;
use crate::steps::typing::coercion::{
    convert_description, convert_expression, convert_many, resolve_type_annotation,
};
use crate::steps::typing::exploration::{Exploration, Links};
use crate::steps::typing::view::{type_to_string, TypeInstance};
use crate::types::engine::CodeEntry;
use crate::types::hir::{ExprKind, TypedExpr};
use crate::types::ty::{FunctionType, MethodType, Parameter, Type, TypeRef};
use crate::types::{ERROR, STRING, UNIT};

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
    pub(super) type_arguments: Vec<TypeRef>,
    /// The converted arguments to pass to the function.
    ///
    /// If any conversion is required, it will be done here.
    pub(super) arguments: Vec<TypedExpr>,

    /// The function identifier to call.
    pub(super) definition: Definition,

    /// The function return type.
    pub(super) return_type: TypeRef,

    /// The function's reef
    pub(super) reef: ReefId,
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
        resolve_type_annotation(exploration, links, return_type_annotation, diagnostics)
    } else {
        UNIT
    };

    let mut typed_return_locations: Vec<_> = Vec::new();

    for ret in &exploration.returns {
        if convert_description(
            exploration,
            expected_return_type,
            ret.ty,
            &mut TypesBounds::inactive(),
            true,
        )
        .is_err()
        {
            typed_return_locations.push(Observation::here(
                links.source,
                exploration.externals.current,
                ret.segment.clone(),
                if func.return_type.is_some() {
                    format!(
                        "Found `{}`",
                        type_to_string(ret.ty, exploration, &TypesBounds::inactive())
                    )
                } else {
                    format!(
                        "Returning `{}`",
                        type_to_string(ret.ty, exploration, &TypesBounds::inactive())
                    )
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
                    exploration.externals.current,
                    return_type_annotation.segment(),
                    format!(
                        "Expected `{}` because of return type",
                        type_to_string(expected_return_type, exploration, &TypesBounds::inactive()),
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
    let unify = convert_many(exploration, &mut TypesBounds::inactive(), types);

    if let Ok(common_type) = unify {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::CannotInfer,
                "Return type inference is not supported yet",
            )
            .with_observation(Observation::context(
                links.source,
                exploration.externals.current,
                segment,
                "No return type is specified",
            ))
            .with_observations(typed_return_locations)
            .with_help(format!(
                "Add -> {} to the function declaration",
                type_to_string(common_type, exploration, &TypesBounds::inactive()),
            )),
        );
    } else {
        diagnostics.push(
            Diagnostic::new(DiagnosticID::CannotInfer, "Failed to infer return type")
                .with_observation(Observation::context(
                    links.source,
                    exploration.externals.current,
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

    let (fun_reef, fun_source) = match call_symbol_ref {
        SymbolRef::Local(_) => (exploration.externals.current, links.source),
        SymbolRef::External(r) => {
            let call_symbol = links.relations[r].state.expect_resolved("unresolved");
            (call_symbol.reef, call_symbol.source)
        }
    };

    let function_type_ref = exploration
        .get_var(fun_source, call_symbol_ref, links.relations)
        .unwrap()
        .type_ref;

    match exploration.get_type(function_type_ref).unwrap() {
        &Type::Function(definition) => {
            let entry: CodeEntry = exploration.get_entry(fun_reef, definition).unwrap();
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
                        exploration.externals.current,
                        call.segment.clone(),
                        "Function is called here",
                    )),
                );
                FunctionMatch {
                    type_arguments: entry.type_parameters().to_vec(),
                    arguments,
                    definition: Definition::error(),
                    return_type,
                    reef: fun_reef,
                }
            } else {
                let type_arguments = entry.type_parameters().to_vec();

                let mut bounds = build_bounds(
                    &call.type_parameters,
                    fun_reef,
                    definition,
                    exploration,
                    links,
                    diagnostics,
                );

                let mut casted_arguments = Vec::with_capacity(parameters.len());
                for (param, arg) in parameters.iter().cloned().zip(arguments) {
                    let param_bound = bounds.get_bound(param.ty);

                    let casted_argument = convert_expression(
                        arg,
                        param_bound,
                        &mut bounds,
                        exploration,
                        links.source,
                        diagnostics,
                    );

                    let casted_argument = match casted_argument {
                        Ok(arg) => {
                            bounds.update_bound(param.ty, arg.ty);
                            arg
                        }
                        Err(arg) => {
                            diagnostics.push(diagnose_arg_mismatch(
                                exploration,
                                links.source,
                                exploration.externals.current,
                                fun_reef,
                                &param,
                                &arg,
                                &bounds,
                            ));
                            arg
                        }
                    };

                    casted_arguments.push(casted_argument);
                }

                let return_type = bounds.get_bound(return_type);

                FunctionMatch {
                    type_arguments,
                    arguments: casted_arguments,
                    definition,
                    return_type,
                    reef: fun_reef,
                }
            }
        }
        _ => {
            let ty = exploration.get_type_instance(function_type_ref);
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    "Cannot invoke non function type",
                )
                .with_observation(Observation::here(
                    links.source,
                    exploration.externals.current,
                    call.segment(),
                    format!(
                        "Call expression requires function, found `{}`",
                        type_to_string(ty.id, exploration, &TypesBounds::inactive())
                    ),
                )),
            );
            FunctionMatch {
                type_arguments: Vec::new(),
                arguments,
                definition: Definition::error(),
                return_type: ERROR,
                reef: fun_reef,
            }
        }
    }
}

fn build_bounds(
    user_bounds: &[ast::r#type::Type],
    fun_reef: ReefId,
    definition: Definition,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypesBounds {
    let user_bounds_types: Vec<_> = user_bounds
        .iter()
        .map(|ty| resolve_type_annotation(exploration, links, ty, diagnostics))
        .collect();

    let entry = exploration.get_entry(fun_reef, definition).unwrap();

    let entry_type_parameters = entry.type_parameters();

    let mut bounds = HashMap::new();

    for (idx, type_param) in entry_type_parameters.iter().enumerate() {
        let user_bound = user_bounds_types.get(idx).cloned();
        bounds.insert(*type_param, user_bound.unwrap_or(*type_param));
    }

    if !user_bounds.is_empty() && user_bounds.len() != entry_type_parameters.len() {
        let first = user_bounds.first().unwrap();
        let last = user_bounds.last().unwrap();

        let segment = first.segment().start..last.segment().end;

        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::InvalidTypeArguments,
                "Wrong type argument count",
            )
            .with_observation(Observation::here(
                links.source,
                exploration.externals.current,
                segment,
                format!(
                    "`{}` type parameter specified, expected `{}`.",
                    user_bounds.len(),
                    entry_type_parameters.len()
                ),
            )),
        )
    }

    TypesBounds::new(bounds)
}

/// Checks the type of a method expression.
pub(super) fn find_operand_implementation(
    exploration: &Exploration,
    reef: ReefId,
    methods: &[MethodType],
    left: TypeRef,
    right: TypedExpr,
) -> Option<FunctionMatch> {
    for method in methods {
        if let [param] = &method.parameters.as_slice() {
            if param.ty == right.ty {
                let return_type = exploration.concretize(method.return_type, left).id;
                return Some(FunctionMatch {
                    type_arguments: vec![method.return_type],
                    arguments: vec![right],
                    definition: method.definition,
                    return_type,
                    reef,
                });
            }
        }
    }
    None
}

/// Checks the type of a method expression.
pub(super) fn type_method(
    method_call: &MethodCall,
    callee: &TypedExpr,
    arguments: Vec<TypedExpr>,
    diagnostics: &mut Vec<Diagnostic>,
    exploration: &mut Exploration,
    source: SourceId,
) -> Option<FunctionMatch> {
    if callee.ty.is_err() {
        return None;
    }

    let current_reef = exploration.externals.current;

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
                        type_to_string(callee.ty, exploration, &TypesBounds::inactive())
                    )
                } else {
                    format!(
                        "Type `{}` is not directly callable",
                        type_to_string(callee.ty, exploration, &TypesBounds::inactive())
                    )
                },
            )
            .with_observation((source, current_reef, method_call.segment.clone()).into()),
        );
        return None;
    }

    let methods = type_methods.unwrap(); // We just checked for None
    let method = find_exact_method(exploration, callee.ty, methods, &arguments);
    if let Some(method) = method {
        // We have an exact match
        return Some(FunctionMatch {
            type_arguments: method.type_parameters.clone(),
            arguments,
            definition: method.definition,
            return_type: exploration.concretize(method.return_type, callee.ty).id,
            reef: callee.ty.reef,
        });
    }

    if methods.len() == 1 {
        // If there is only one method, we can give a more specific error by adding
        // an observation for each invalid type
        let method = methods.first().unwrap();

        // release borrow of exploration
        // TODO avoid clone
        let method = method.clone();

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
                    current_reef,
                    method_call.segment(),
                    "Method is called here",
                ))
                .with_help(format!(
                    "The method signature is `{}::{}`",
                    type_to_string(callee.ty, exploration, &TypesBounds::inactive()),
                    Signature::new(exploration, method_name, &method)
                )),
            );
        } else {
            // cannot use `build_bounds` as it implies to retrieve a native chunk from its definition, which is
            // completely broken currently

            let mut bounds = TypesBounds::new(
                method
                    .type_parameters
                    .iter()
                    .map(|p| (*p, exploration.concretize(*p, callee.ty).id))
                    .collect(),
            );

            for (param, arg) in method.parameters.iter().zip(arguments.iter()) {
                let param_bound = bounds.get_bound(param.ty);

                match convert_description(exploration, param_bound, arg.ty, &mut bounds, true) {
                    Ok(ty) => {
                        bounds.update_bound(param.ty, ty);
                    }
                    Err(_) => {
                        let param = Parameter {
                            location: param.location.clone(),
                            ty: param_bound,
                            local_id: param.local_id,
                        };
                        let diagnostic = diagnose_arg_mismatch(
                            exploration,
                            source,
                            current_reef,
                            callee.ty.reef,
                            &param,
                            arg,
                            &bounds,
                        )
                        .with_observation(Observation::here(
                            source,
                            current_reef,
                            method_call.segment(),
                            "Arguments to this method are incorrect",
                        ));
                        diagnostics.push(diagnostic);
                    }
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
                    type_to_string(callee.ty, exploration, &TypesBounds::inactive())
                ),
            )
            .with_observation(Observation::here(
                source,
                current_reef,
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
    current_reef: ReefId,
    param_reef: ReefId,
    param: &Parameter,
    arg: &TypedExpr,
    bounds: &TypesBounds,
) -> Diagnostic {
    let diagnostic = Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch").with_observation(
        Observation::here(
            source,
            current_reef,
            arg.segment.clone(),
            format!(
                "Expected `{}`, found `{}`",
                type_to_string(param.ty, exploration, bounds),
                type_to_string(arg.ty, exploration, bounds)
            ),
        ),
    );
    if let Some(location) = &param.location {
        diagnostic.with_observation(Observation::context(
            location.source,
            param_reef,
            location.segment.clone(),
            "Parameter is declared here",
        ))
    } else {
        diagnostic
    }
}

/// Find a matching method for the given arguments.
fn find_exact_method<'a>(
    exploration: &Exploration,
    obj: TypeRef,
    methods: &'a [MethodType],
    args: &[TypedExpr],
) -> Option<&'a MethodType> {
    for method in methods {
        if method.parameters.len() != args.len() {
            continue;
        }
        let mut matches = true;
        for (param, arg) in method.parameters.iter().zip(args.iter()) {
            let param_type = exploration.concretize(param.ty, obj).id;
            if param_type != arg.ty {
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
    local_id: LocalId,
    exploration: &mut Exploration,
    param: &FunctionParameter,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
) -> Parameter {
    match param {
        FunctionParameter::Named(named) => {
            let type_id = named.ty.as_ref().map_or(STRING, |ty| {
                resolve_type_annotation(exploration, links, ty, diagnostics)
            });
            Parameter {
                location: Some(SourceLocation::new(
                    links.source,
                    exploration.externals.current,
                    named.segment.clone(),
                )),
                ty: type_id,
                local_id,
            }
        }
        FunctionParameter::Slf(_) => todo!("method not supported yet"),
        FunctionParameter::Variadic(_, _) => todo!("Arrays are not supported yet"),
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
    exploration: &'a Exploration<'a>,
    name: &'a str,
    function: &'a FunctionType,
}

impl<'a> Signature<'a> {
    /// Creates a new signature.
    fn new(exploration: &'a Exploration<'a>, name: &'a str, function: &'a FunctionType) -> Self {
        Self {
            exploration,
            name,
            function,
        }
    }

    fn get_type(&self, id: TypeRef) -> TypeInstance {
        self.exploration.get_type_instance(id)
    }
}

impl fmt::Display for Signature<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.name)?;
        if let Some((first, parameters)) = self.function.parameters.split_first() {
            write!(
                f,
                "{}",
                type_to_string(first.ty, self.exploration, &TypesBounds::inactive())
            )?;
            for param in parameters {
                write!(
                    f,
                    ", {}",
                    type_to_string(param.ty, self.exploration, &TypesBounds::inactive())
                )?;
            }
        }
        if self.function.return_type.is_nothing() {
            write!(f, ")")
        } else {
            write!(
                f,
                ") -> {}",
                type_to_string(
                    self.function.return_type,
                    self.exploration,
                    &TypesBounds::inactive()
                )
            )
        }
    }
}
