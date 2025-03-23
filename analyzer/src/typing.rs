mod assign;
mod flow;
pub mod function;
mod iterable;
mod lower;
mod operator;
mod pfc;
pub mod registry;
pub mod schema;
mod shell;
pub mod user;
pub mod variable;

use crate::hir::{Conditional, Declaration, ExprKind, Module, TypedExpr};
use crate::import::{PathEntry, PathItemError, SymbolSearch};
use crate::module::ModuleView;
use crate::symbol::{Symbol, SymbolDesc, SymbolRegistry};
use crate::typing::assign::{
    ascribe_assign, ascribe_identifier, ascribe_subscript, ascribe_var_reference,
};
use crate::typing::flow::{ascribe_control, ascribe_while};
use crate::typing::function::Function;
use crate::typing::iterable::{ascribe_for, ascribe_range};
use crate::typing::lower::{
    ascribe_template_string, coerce_condition, lower_implicit_cast, Implicit,
};
use crate::typing::operator::{ascribe_binary, ascribe_unary};
use crate::typing::pfc::ascribe_pfc;
use crate::typing::registry::{Registry, SchemaId};
use crate::typing::schema::{ascribe_field_access, Schema};
use crate::typing::shell::{ascribe_call, ascribe_detached, ascribe_file_pattern, ascribe_pipeline, ascribe_redirected, ascribe_subshell, ascribe_substitution, ascribe_tilde};
use crate::typing::user::{
    lookup_builtin_type, TypeArena, TypeId, UserType, BOOL_TYPE, ERROR_TYPE, FLOAT_TYPE, INT_TYPE,
    NOTHING_TYPE, STRING_TYPE, UNIT_TYPE, UNKNOWN_TYPE,
};
use crate::typing::variable::{SymbolEntry, VariableTable};
use crate::{Database, PipelineError, Reef, SourceLocation, UnitKey};
use ast::call::MethodCall;
use ast::control_flow::If;
use ast::function::FunctionDeclaration;
use ast::group::Block;
use ast::r#struct::StructImpl;
use ast::r#type::{ByName, ParametrizedType, Type};
use ast::r#use::{Import, ImportList, InclusionPathItem, Use};
use ast::range::Iterable;
use ast::value::{Literal, LiteralValue};
use ast::variable::{VarDeclaration, VarKind};
use ast::Expr;
use context::source::{SourceSegmentHolder, Span};
use std::ffi::OsStr;
use std::path::PathBuf;
use thiserror::Error;

pub(super) fn type_check(
    reef: &mut Reef,
    Database {
        exports,
        ref mut checker,
    }: &mut Database,
    sorted: Vec<UnitKey>,
) -> Vec<TypeError> {
    let mut errors = Vec::<TypeError>::new();
    for key in sorted {
        let root = reef.files.get(&key).expect("file should be present");
        let mut table = VariableTable::new(
            reef.symbols
                .get_mut(key.path.as_path())
                .expect("table should be present"),
        );
        let mut current_module = Module::new(key.path.clone());
        ascribe_types(
            root,
            &mut table,
            checker,
            &mut current_module,
            ModuleView::new(&reef.exports, exports),
            &mut errors,
        );
        current_module.exports = table.take_exports();
        let all_module_exports = reef
            .exports
            .get_full_mut(&key.path)
            .expect("module should exist");
        for (variable, ty) in &current_module.exports {
            if let Some(hoisted_export) = all_module_exports.exports.iter_mut().find(|export| {
                export.name == variable.as_str() && export.registry == SymbolRegistry::Variable
            }) {
                hoisted_export.ty = *ty;
            }
        }
        reef.hir.push(current_module);
    }
    errors
}

/// A structure that holds the different type information.
#[derive(Default)]
pub struct TypeChecker {
    pub types: TypeArena,
    pub registry: Registry,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub ty: TypeId,
    pub span: Span,
}

impl TypeChecker {
    fn display(&self, ty: TypeId) -> String {
        match &self.types[ty] {
            UserType::Unknown => "Unknown".to_string(),
            UserType::Error => "Error".to_string(),
            UserType::Nothing => "Nothing".to_string(),
            UserType::Unit => "Unit".to_string(),
            UserType::Function(function) => {
                let Function {
                    param_types,
                    return_type,
                    ..
                } = &self.registry[*function];
                let params = param_types
                    .iter()
                    .map(|param| self.display(param.ty))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({params}) -> {}", self.display(*return_type))
            }
            UserType::Parametrized { schema, params } => {
                let Schema {
                    name,
                    generic_variables,
                    ..
                } = &self.registry[*schema];
                let params = params
                    .iter()
                    .map(|ty| self.display(*ty))
                    .collect::<Vec<_>>()
                    .join(", ");
                if generic_variables.is_empty() {
                    name.to_owned()
                } else {
                    format!("{name}[{params}]")
                }
            }
            UserType::Module(path) => path
                .iter()
                .map(|item| item.to_string())
                .collect::<Vec<_>>()
                .join("::"),
            UserType::GenericVariable(name) => name.clone(),
        }
    }
}

pub(crate) struct UnifyError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub at: SourceLocation,
}

impl TypeError {
    pub fn new(kind: TypeErrorKind, at: SourceLocation) -> Self {
        Self { kind, at }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErroneousSymbolDesc {
    Partial(PathEntry),
    Complete(SymbolDesc),
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum TypeErrorKind {
    #[error("undefined {expected} `{name}`")]
    UndefinedSymbol {
        name: String,
        expected: SymbolRegistry,
        found: Option<ErroneousSymbolDesc>,
    },

    #[error("duplicate symbol `{name}`")]
    DuplicateSymbol { name: String, previous: Span },

    #[error("missing type")]
    MissingType,

    #[error("type mismatch, expected `{expected}`, received `{actual}`")]
    TypeMismatch {
        expected: String,
        expected_due_to: Option<SourceLocation>,
        actual: String,
    },

    #[error("trait `{trait_name}` not implemented for type `{type_name}`")]
    TraitNotImplemented {
        trait_name: String,
        type_name: String,
    },

    #[error("expected {expected} arguments but received {received}")]
    ArityMismatch { expected: usize, received: usize },

    #[error("no field `{name}` on type `{type_name}`")]
    UnknownField {
        name: String,
        type_name: String,
        available: Vec<String>,
    },

    #[error("no method `{name}` on type `{type_name}`")]
    UnknownMethod { name: String, type_name: String },

    #[error("type annotation needed")]
    TypeAnnotationRequired {
        types: Vec<String>,
        insert_at: usize,
    },

    #[error("return statement outside of function body")]
    ReturnOutsideFunction,

    #[error("loop control statement outside of a loop")]
    ControlOutsideLoop,

    #[error("repeated parameter name `{name}`")]
    RepeatedParameterName { name: String, previous: Span },

    #[error("`self` parameter is only allowed in methods")]
    UnexpectedSelfParameter,

    #[error("cannot define an implementation for primitive types")]
    CannotImplPrimitive,

    #[error("attempted to a access a method like a field")]
    MethodLikeFieldAccess { name: String, parentheses: String },

    #[error("cannot assign twice to immutable variable `{name}`")]
    CannotReassign { name: String },

    #[error("found circular dependency")]
    CircularDependency { cycle: Vec<PathBuf> },
}

impl From<TypeError> for PipelineError {
    fn from(err: TypeError) -> Self {
        PipelineError::Type(err)
    }
}

/// Informs the type inference algorithm about the locally expected type.
///
/// Note that it is only informative, and it should not be enforced, i.e. no error should be raised
/// if the type is not the expected one. Such checks should be made by the receiver of the type and
/// not the producer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeHint {
    /// The return type is immediately discarded.
    ///
    /// Subbranches are not required to have a specific type and may return different types.
    Unused,

    /// The return type is used, but not required to be a specific type.
    ///
    /// It forces subexpressions to coerce to an exact common type.
    Used,

    /// The type is used and is expected to be a specific type.
    ///
    /// If the expression calls a generic function, this type may be used during the type inference
    /// if not provided explicitly for instance.
    Required(TypeId),
}

impl TypeHint {
    fn is_used(self) -> bool {
        matches!(self, TypeHint::Used | TypeHint::Required(_))
    }
}

#[derive(Clone, Copy)]
struct Context<'a> {
    modules: ModuleView<'a>,
    hint: TypeHint,
    return_ty: Option<&'a Return>,
    in_loop: bool,
}

#[derive(Clone)]
struct Return {
    ty: TypeId,
    span: Span,
}

impl<'a> Context<'a> {
    fn with_hint(self, hint: TypeHint) -> Self {
        Self { hint, ..self }
    }

    fn with_return(self, return_ty: &'a Return) -> Self {
        Self {
            return_ty: Some(return_ty),
            ..self
        }
    }

    fn in_loop(self) -> Self {
        Self {
            in_loop: true,
            ..self
        }
    }
}

pub(super) fn ascribe_types(
    root: &[Expr],
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    modules: ModuleView,
    errors: &mut Vec<TypeError>,
) {
    let mut expressions = Vec::new();
    table.push_environment();
    for expr in root.iter() {
        let ctx = Context {
            modules,
            hint: TypeHint::Unused,
            return_ty: None,
            in_loop: false,
        };
        expressions.push(ascribe_type(expr, table, checker, storage, ctx, errors));
    }
    let hir = TypedExpr {
        kind: ExprKind::Block(expressions),
        span: 0..0,
        ty: UNIT_TYPE,
    };
    storage.add(None, hir, table.pop_environment());
}

fn ascribe_type(
    expr: &Expr,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx @ Context { modules, hint, .. }: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    match expr {
        Expr::Use(Use {
            import,
            segment: span,
        }) => {
            ascribe_import(import, table, checker, modules, errors);
            TypedExpr::noop(span.clone())
        }
        Expr::Literal(Literal {
            parsed,
            segment: span,
        }) => TypedExpr {
            kind: ExprKind::Literal(parsed.clone()),
            span: span.clone(),
            ty: match parsed {
                LiteralValue::String(_) => STRING_TYPE,
                LiteralValue::Int(_) => INT_TYPE,
                LiteralValue::Float(_) => FLOAT_TYPE,
                LiteralValue::Bool(_) => BOOL_TYPE,
            },
        },
        Expr::Unary(unary) => ascribe_unary(unary, table, checker, storage, ctx, errors),
        Expr::Binary(binary) => ascribe_binary(binary, table, checker, storage, ctx, errors),
        Expr::TemplateString(tpl) => {
            ascribe_template_string(tpl, table, checker, storage, ctx, errors)
        }
        Expr::VarDeclaration(VarDeclaration {
            kind,
            var,
            initializer: Some(initializer),
            segment: span,
        }) => {
            let expected_ty = var
                .ty
                .as_ref()
                .map(|ty| lookup_type(ty, table, checker, modules, errors));
            let ctx = ctx.with_hint(expected_ty.map_or(TypeHint::Used, TypeHint::Required));
            let mut typed_initializer =
                ascribe_type(initializer, table, checker, storage, ctx, errors);
            let mut ty = typed_initializer.ty;
            if typed_initializer.is_ok() {
                if let Some(expected_ty) = expected_ty {
                    typed_initializer = lower_implicit_cast(
                        typed_initializer,
                        Implicit {
                            assign_to: expected_ty,
                            expected_due_to: var.ty.as_ref().map(|ty| {
                                SourceLocation::new(
                                    table.path().to_owned(),
                                    var.ty.as_ref().unwrap().segment(),
                                )
                            }),
                        },
                        checker,
                        table.path(),
                        errors,
                    );
                    ty = expected_ty;
                }
            }
            let var = table.insert_variable(
                var.name.value.to_string(),
                ty,
                var.name.segment(),
                *kind == VarKind::Var,
            );
            TypedExpr {
                kind: ExprKind::Declare(Declaration {
                    identifier: var.clone(),
                    value: Some(Box::new(typed_initializer)),
                }),
                span: span.clone(),
                ty: UNIT_TYPE,
            }
        }
        Expr::Assign(assign) => ascribe_assign(assign, table, checker, storage, ctx, errors),
        Expr::Subscript(subscript) => {
            ascribe_subscript(subscript, table, checker, storage, ctx, errors)
        }
        Expr::While(stmt) => ascribe_while(stmt, table, checker, storage, ctx, errors),
        Expr::For(stmt) => ascribe_for(stmt, table, checker, storage, ctx, errors),
        Expr::Break(span) => ascribe_control(ExprKind::Break, span.clone(), table, ctx, errors),
        Expr::Continue(span) => {
            ascribe_control(ExprKind::Continue, span.clone(), table, ctx, errors)
        }
        Expr::FunctionDeclaration(fn_decl) => {
            ascribe_fn_decl(fn_decl, None, table, checker, storage, ctx, errors);
            TypedExpr::noop(fn_decl.segment())
        }
        Expr::Call(call) => ascribe_call(call, table, checker, storage, ctx, errors),
        Expr::Substitution(sub) => ascribe_substitution(sub, table, checker, storage, ctx, errors),
        Expr::Parenthesis(paren) => {
            ascribe_type(&paren.expression, table, checker, storage, ctx, errors)
        }
        Expr::StructDeclaration(decl) => TypedExpr::noop(decl.segment.clone()),
        Expr::ProgrammaticCall(call) => ascribe_pfc(call, table, checker, storage, ctx, errors),
        Expr::FieldAccess(expr) => ascribe_field_access(expr, table, checker, storage, ctx, errors),
        Expr::VarReference(ident) => ascribe_var_reference(ident, table, errors),
        Expr::Path(ident) => ascribe_identifier(ident, table, errors),
        Expr::Block(Block {
            expressions,
            segment: span,
        }) => {
            table.enter_scope();
            let expressions = expressions
                .iter()
                .map(|expr| ascribe_type(expr, table, checker, storage, ctx, errors))
                .collect::<Vec<TypedExpr>>();
            table.exit_scope();
            let last_ty = expressions.last().map(|expr| expr.ty).unwrap_or(UNIT_TYPE);
            TypedExpr {
                kind: ExprKind::Block(expressions),
                span: span.clone(),
                ty: last_ty,
            }
        }
        Expr::Redirected(redirected) => {
            ascribe_redirected(redirected, table, checker, storage, ctx, errors)
        }
        Expr::Detached(detached) => {
            ascribe_detached(detached, table, checker, storage, ctx, errors)
        }
        Expr::Pipeline(pipeline) => {
            ascribe_pipeline(pipeline, table, checker, storage, ctx, errors)
        }
        Expr::Subshell(subshell) => {
            ascribe_subshell(subshell, table, checker, storage, ctx, errors)
        }
        Expr::Tilde(tilde) => ascribe_tilde(tilde, table, checker, storage, ctx, errors),
        Expr::Range(iterable) => match iterable {
            Iterable::Range(range) => ascribe_range(range, table, checker, storage, ctx, errors),
            Iterable::Files(pattern) => {
                ascribe_file_pattern(pattern, table, checker, storage, ctx, errors)
            }
        },
        Expr::If(If {
            condition,
            success_branch,
            fail_branch,
            segment: span,
        }) => {
            let typed_condition = ascribe_type(condition, table, checker, storage, ctx, errors);
            let typed_condition = coerce_condition(typed_condition, table, checker, errors);
            if let Err(_) = checker.types.unify(typed_condition.ty, BOOL_TYPE) {
                errors.push(TypeError::new(
                    TypeErrorKind::TypeMismatch {
                        expected: "Bool".to_owned(),
                        expected_due_to: None,
                        actual: checker.display(typed_condition.ty),
                    },
                    SourceLocation::new(table.path().to_owned(), condition.segment()),
                ));
            }
            let then_branch = ascribe_type(success_branch, table, checker, storage, ctx, errors);
            let otherwise_branch = fail_branch
                .as_ref()
                .map(|branch| ascribe_type(branch, table, checker, storage, ctx, errors))
                .unwrap_or_else(|| TypedExpr::noop(span.clone()));
            if hint.is_used() && then_branch.ty != otherwise_branch.ty {
                errors.push(TypeError::new(
                    TypeErrorKind::TypeMismatch {
                        expected: checker.display(then_branch.ty),
                        expected_due_to: Some(SourceLocation::new(
                            table.path().to_owned(),
                            success_branch.segment(),
                        )),
                        actual: checker.display(otherwise_branch.ty),
                    },
                    SourceLocation::new(table.path().to_owned(), span.clone()),
                ));
            }
            let ty = then_branch.ty;
            TypedExpr {
                kind: ExprKind::Conditional(Conditional {
                    condition: Box::new(typed_condition),
                    then: Box::new(then_branch),
                    otherwise: Some(Box::new(otherwise_branch)),
                }),
                span: span.clone(),
                ty,
            }
        }
        Expr::Return(ast::function::Return {
            expr,
            segment: span,
        }) => {
            let typed_expr = expr
                .as_ref()
                .map(|expr| ascribe_type(expr, table, checker, storage, ctx, errors));
            let ty = typed_expr.as_ref().map_or(UNIT_TYPE, |expr| expr.ty);
            if let Some(Return {
                ty: return_ty,
                span: return_span,
            }) = ctx.return_ty
            {
                if let Err(_) = checker.types.unify(ty, *return_ty) {
                    errors.push(TypeError::new(
                        TypeErrorKind::TypeMismatch {
                            expected: checker.display(*return_ty),
                            expected_due_to: Some(SourceLocation::new(
                                table.path().to_owned(),
                                return_span.clone(),
                            )),
                            actual: checker.display(ty),
                        },
                        SourceLocation::new(table.path().to_owned(), span.clone()),
                    ));
                }
            } else {
                errors.push(TypeError::new(
                    TypeErrorKind::ReturnOutsideFunction,
                    SourceLocation::new(table.path().to_owned(), span.clone()),
                ));
            }
            TypedExpr {
                kind: ExprKind::Return(typed_expr.map(Box::new)),
                span: span.clone(),
                ty: NOTHING_TYPE,
            }
        }
        Expr::Impl(StructImpl {
            type_parameters,
            impl_type,
            functions,
            segment: span,
        }) => {
            table.enter_scope();
            for type_param in type_parameters.iter() {
                table.insert_local(
                    type_param.name.to_string(),
                    UNKNOWN_TYPE,
                    type_param.segment.clone(),
                    SymbolEntry::Type,
                );
            }
            let self_ty = lookup_type(impl_type, table, checker, modules, errors);
            for function in functions {
                ascribe_fn_decl(
                    function,
                    Some(self_ty),
                    table,
                    checker,
                    storage,
                    ctx,
                    errors,
                );
            }
            table.exit_scope();
            TypedExpr::noop(span.clone())
        }
        Expr::MethodCall(MethodCall {
            source,
            name: ident,
            arguments,
            type_parameters,
            segment: span,
        }) => {
            let typed_source = ascribe_type(source, table, checker, storage, ctx, errors);
            if typed_source.ty.is_err() {
                return TypedExpr::error(span.clone());
            }
            let type_parameters = type_parameters
                .iter()
                .map(|type_param| lookup_type(type_param, table, checker, modules, errors))
                .collect::<Vec<TypeId>>();
            let args = arguments
                .iter()
                .map(|expr| ascribe_type(expr, table, checker, storage, ctx, errors))
                .collect::<Vec<_>>();
            let name = ident.as_ref().map_or("apply", |name| name.value.as_str());
            match &checker.types[typed_source.ty] {
                UserType::Parametrized { schema, params } => {
                    let Schema {
                        fields, methods, ..
                    } = &checker.registry[*schema];
                    if let Some(method) = methods.get(name) {
                        let Function {
                            ref generic_variables,
                            return_type,
                            ..
                        } = checker.registry[*method];
                        let type_parameters = {
                            let mut parameters = params.clone();
                            parameters.extend(type_parameters);
                            parameters
                        };
                        let return_type = checker.types.concretize(
                            return_type,
                            generic_variables,
                            &type_parameters,
                        );
                        TypedExpr {
                            kind: ExprKind::MethodCall(crate::hir::MethodCall {
                                callee: Box::new(typed_source),
                                arguments: args,
                                function_id: *method,
                            }),
                            span: span.clone(),
                            ty: return_type,
                        }
                    } else if let Some(_) = fields.iter().find(|field| field.name == name) {
                        errors.push(TypeError::new(
                            TypeErrorKind::MethodLikeFieldAccess {
                                name: name.to_string(),
                                parentheses: "()".to_string(),
                            },
                            SourceLocation::new(table.path().to_owned(), span.clone()),
                        ));
                        TypedExpr::error(span.clone())
                    } else {
                        errors.push(TypeError::new(
                            TypeErrorKind::UnknownField {
                                name: name.to_string(),
                                type_name: checker.display(typed_source.ty),
                                available: Vec::new(),
                            },
                            SourceLocation::new(
                                table.path().to_owned(),
                                ident.as_ref().map_or(span.clone(), |ident| ident.segment()),
                            ),
                        ));
                        TypedExpr::error(span.clone())
                    }
                }
                _ => {
                    errors.push(TypeError::new(
                        TypeErrorKind::TypeMismatch {
                            expected: "Struct".to_string(),
                            expected_due_to: None,
                            actual: checker.display(typed_source.ty),
                        },
                        SourceLocation::new(table.path().to_owned(), span.clone()),
                    ));
                    TypedExpr::error(span.clone())
                }
            }
        }
        expr => todo!("{expr:?}"),
    }
}

fn ascribe_fn_decl(
    FunctionDeclaration {
        name: ident,
        type_parameters,
        parameters,
        return_type: return_ty_ident,
        body,
        segment: span,
    }: &FunctionDeclaration,
    current_ty: Option<TypeId>,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) {
    table.push_environment();
    let function = match current_ty {
        Some(ty) => {
            let UserType::Parametrized { schema, .. } = checker.types[ty] else {
                panic!(
                    "function should have a struct type, got {:?}",
                    checker.types[ty]
                );
            };
            let Schema {
                ref mut methods, ..
            } = checker.registry[schema];
            *methods
                .get(ident.value.as_str())
                .expect("method should be defined in the struct")
        }
        None => {
            let Symbol { ty, .. } = table
                .get(&ident.value, SymbolRegistry::Function)
                .expect("function should be defined in the table");
            let UserType::Function(function) = checker.types[*ty] else {
                panic!(
                    "function should have a function type, got {:?}",
                    checker.types[*ty]
                );
            };
            function
        }
    };
    let Function {
        ref generic_variables,
        ref param_types,
        return_type,
        ..
    } = checker.registry[function];
    table.enter_scope();
    storage.enter_namespace(ident.value.as_str());
    for type_param in type_parameters.iter() {
        table.insert_local(
            type_param.name.to_string(),
            UNKNOWN_TYPE,
            type_param.segment.clone(),
            SymbolEntry::Type,
        );
    }
    for (i, (param, param_ty)) in parameters.iter().zip(param_types.iter()).enumerate() {
        if let Some(previous) = parameters[..i]
            .iter()
            .find(|prev| prev.name() == param.name())
        {
            errors.push(TypeError::new(
                TypeErrorKind::RepeatedParameterName {
                    name: param.name().to_owned(),
                    previous: previous.segment().clone(),
                },
                SourceLocation::new(table.path().to_owned(), param.segment()),
            ));
        }
        table.insert_variable(param.name().to_owned(), param_ty.ty, span.clone(), false);
    }
    if let Some(body) = body.as_ref() {
        let ret = Return {
            ty: return_type,
            span: match return_ty_ident {
                None => ident.segment(),
                Some(ref ret) => ret.segment(),
            },
        };
        let ctx = ctx.with_return(&ret);
        let typed_body = ascribe_type(body, table, checker, storage, ctx, errors);
        if let Err(_) = checker.types.unify(typed_body.ty, return_type) {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: checker.display(return_type),
                    expected_due_to: return_ty_ident
                        .as_ref()
                        .map(|ty| SourceLocation::new(table.path().to_owned(), ty.segment())),
                    actual: checker.display(typed_body.ty),
                },
                SourceLocation::new(table.path().to_owned(), body.segment()),
            ));
        }
        storage.add(Some(function), typed_body, table.pop_environment());
    }
    storage.exit_namespace();
    table.exit_scope();
}

fn lookup_type(
    ty: &Type,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    modules: ModuleView,
    errors: &mut Vec<TypeError>,
) -> TypeId {
    match ty {
        Type::Parametrized(ParametrizedType {
            path,
            params,
            segment: span,
        }) => {
            // let ty = lookup_path(path, SymbolRegistry::Type, table, checker, modules, errors);
            let ty = if let [InclusionPathItem::Symbol(item)] = path.as_slice() {
                lookup_builtin_type(item.value.as_str()).unwrap_or_else(|| {
                    lookup_path(path, SymbolRegistry::Type, table, checker, modules, errors)
                })
            } else {
                lookup_path(path, SymbolRegistry::Type, table, checker, modules, errors)
            };
            if ty.is_err() {
                return ERROR_TYPE;
            }
            let type_params = params
                .iter()
                .map(|ty| lookup_type(ty, table, checker, modules, errors))
                .collect::<Vec<TypeId>>();
            let mut schema: Option<SchemaId> = None;
            let generic_variables = match &checker.types[ty] {
                UserType::Parametrized {
                    schema: found_schema,
                    ..
                } => {
                    let Schema {
                        generic_variables, ..
                    } = &checker.registry[*found_schema];
                    schema = Some(*found_schema);
                    generic_variables.as_slice()
                }
                _ => &[],
            };
            if generic_variables.len() != params.len() {
                errors.push(TypeError::new(
                    TypeErrorKind::ArityMismatch {
                        expected: generic_variables.len(),
                        received: params.len(),
                    },
                    SourceLocation::new(
                        table.path().to_owned(),
                        if let Some((first, last)) = params.first().zip(params.last()) {
                            first.segment().start..last.segment().end
                        } else {
                            span.clone()
                        },
                    ),
                ));
                ty
            } else if generic_variables == type_params {
                ty
            } else {
                let Some(schema) = schema else {
                    return ERROR_TYPE;
                };
                checker.types.alloc(UserType::Parametrized {
                    schema,
                    params: type_params,
                })
            }
        }
        Type::Callable(_) => todo!(),
        Type::ByName(ByName { name: ty, .. }) => lookup_type(ty, table, checker, modules, errors),
    }
}

fn lookup_path(
    path: &[InclusionPathItem],
    registry: SymbolRegistry,
    table: &mut VariableTable,
    checker: &TypeChecker,
    modules: ModuleView,
    errors: &mut Vec<TypeError>,
) -> TypeId {
    match SymbolSearch::new(path, &checker.types, modules, table)
        .and_then(|search| search.lookup(registry))
    {
        Ok(search) => search,
        Err(PathItemError { item, err }) => {
            errors.push(TypeError::new(
                TypeErrorKind::UndefinedSymbol {
                    name: item.to_string(),
                    expected: if path.last() == Some(item) {
                        registry
                    } else {
                        SymbolRegistry::Type
                    },
                    found: err.into(),
                },
                SourceLocation::new(table.path().to_owned(), item.segment()),
            ));
            ERROR_TYPE
        }
    }
}

fn ascribe_import(
    import: &Import,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    modules: ModuleView,
    errors: &mut Vec<TypeError>,
) {
    match import {
        Import::Symbol(item) => {
            let (first, rest) = item.path.split_first().expect("path should not be empty");
            let Some(mut tree) = modules.get(first) else {
                errors.push(TypeError::new(
                    TypeErrorKind::UndefinedSymbol {
                        name: first.to_string(),
                        expected: SymbolRegistry::Type,
                        found: None,
                    },
                    SourceLocation::new(table.path().to_owned(), first.segment()),
                ));
                return;
            };
            let Some((last, rest)) = rest.split_last() else {
                return;
            };
            for item in rest {
                let InclusionPathItem::Symbol(ident) = item else {
                    errors.push(TypeError::new(
                        TypeErrorKind::UndefinedSymbol {
                            name: item.to_string(),
                            expected: SymbolRegistry::Type,
                            found: None,
                        },
                        SourceLocation::new(table.path().to_owned(), item.segment()),
                    ));
                    return;
                };
                match tree.get(OsStr::new(ident.value.as_str())) {
                    Some(child_tree) => tree = child_tree,
                    None => {
                        errors.push(TypeError::new(
                            TypeErrorKind::UndefinedSymbol {
                                name: item.to_string(),
                                expected: SymbolRegistry::Type,
                                found: None,
                            },
                            SourceLocation::new(table.path().to_owned(), item.segment()),
                        ));
                        return;
                    }
                }
            }
            match last {
                InclusionPathItem::Symbol(ident) => {
                    let mut found = false;
                    for export in tree.exports.iter() {
                        if export.name != ident.value {
                            continue;
                        }
                        found = true;
                        table.insert_remote(ident.value.to_string(), ident.segment(), export);
                    }
                    for child in tree.children.iter() {
                        if child.name != OsStr::new(&ident.value) {
                            continue;
                        }
                        found = true;
                        table.insert_local(
                            ident.value.to_string(),
                            checker.types.alloc(UserType::Module(item.path.clone())),
                            ident.segment(),
                            SymbolEntry::Type,
                        );
                    }
                    if !found {
                        errors.push(TypeError::new(
                            TypeErrorKind::UndefinedSymbol {
                                name: ident.value.to_string(),
                                expected: SymbolRegistry::Type,
                                found: None,
                            },
                            SourceLocation::new(table.path().to_owned(), ident.segment()),
                        ));
                    }
                }
                InclusionPathItem::Reef(span) => {
                    errors.push(TypeError::new(
                        TypeErrorKind::UndefinedSymbol {
                            name: last.to_string(),
                            expected: SymbolRegistry::Type,
                            found: None,
                        },
                        SourceLocation::new(table.path().to_owned(), span.clone()),
                    ));
                }
            }
        }
        Import::AllIn(path, span) => {
            let tree = modules.get_direct(path).expect("path should be defined");
            for export in tree.exports.iter() {
                table.insert_remote(export.name.clone(), span.clone(), export);
            }
        }
        Import::Environment(_) => {}
        Import::List(ImportList {
            root,
            imports,
            segment: span,
        }) => {
            // FIXME: a bit messy and should prefer tree.exports instead of tree.children
            let mut tree = modules.get_direct(root).expect("root should be defined");
            for import in imports {
                match import {
                    Import::Symbol(symbol) => {
                        let mut iter = symbol.path.iter();
                        while let Some(path) = iter.next() {
                            let InclusionPathItem::Symbol(ident) = path else {
                                panic!("path should be a symbol");
                            };
                            match tree.get(OsStr::new(ident.value.as_str())) {
                                Some(child_tree) => tree = child_tree,
                                None => {
                                    let mut found = false;
                                    if iter.next().is_none() {
                                        for export in tree.exports.iter() {
                                            if export.name != ident.value {
                                                continue;
                                            }
                                            found = true;
                                            table.insert_remote(
                                                ident.value.to_string(),
                                                ident.segment(),
                                                export,
                                            );
                                        }
                                    }
                                    if !found {
                                        errors.push(TypeError::new(
                                            TypeErrorKind::UndefinedSymbol {
                                                name: ident.value.to_string(),
                                                expected: SymbolRegistry::Type,
                                                found: None,
                                            },
                                            SourceLocation::new(
                                                table.path().to_owned(),
                                                ident.segment(),
                                            ),
                                        ));
                                    }
                                    break;
                                }
                            }
                        }
                    }
                    Import::AllIn(_, _) => todo!(),
                    Import::Environment(_) => {}
                    Import::List(_) => todo!(),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hoist::hoist_files;
    use crate::module::import_multi;
    use crate::{Database, MemoryFilesystem};
    use std::collections::HashMap;
    use std::ffi::OsString;
    use std::path::PathBuf;

    pub(crate) fn type_check(source: &str) -> Vec<TypeError> {
        let fs = MemoryFilesystem::new(HashMap::from([(PathBuf::from("main.msh"), source)]));
        check(fs, "main.msh")
    }

    fn check(fs: MemoryFilesystem, entrypoint: &str) -> Vec<TypeError> {
        let mut database = Database::default();
        let mut reef = Reef::new(OsString::from("test"));
        let import_result = import_multi(&mut reef, &fs, entrypoint);
        assert_eq!(import_result.errors, [], "no import errors should be found");
        let hoist_result = hoist_files(
            &database.exports,
            &mut reef,
            &mut database.checker,
            import_result.keys,
        );
        assert_eq!(
            hoist_result.errors,
            [],
            "no hoisting errors should be found"
        );
        super::type_check(&mut reef, &mut database, hoist_result.sorted)
    }

    fn type_check_multi<const N: usize>(sources: [(PathBuf, &str); N]) -> Vec<TypeError> {
        let entrypoint = sources
            .first()
            .expect("at least one source")
            .0
            .display()
            .to_string();
        check(
            MemoryFilesystem::from_iter(sources.into_iter()),
            &entrypoint,
        )
    }

    #[test]
    fn valid_var_type_annotation() {
        let errors = type_check("val x: Int = 1");
        assert_eq!(errors, []);
    }

    #[test]
    fn invalid_var_type_annotation() {
        let errors = type_check("val x: Int = true");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "Int".to_owned(),
                    expected_due_to: Some(SourceLocation::new(PathBuf::from("main.msh"), 7..10)),
                    actual: "Bool".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 13..17),
            )]
        );
    }

    #[test]
    fn analyze_unresolved_back_import() {
        let errors = type_check_multi([
            (PathBuf::from("a"), "use reef::b::b"),
            (PathBuf::from("b"), "use reef::a::a\nfun b() = {}"),
        ]);
        assert_eq!(
            errors,
            vec![TypeError::new(
                TypeErrorKind::UndefinedSymbol {
                    name: "a".to_string(),
                    expected: SymbolRegistry::Type,
                    found: None,
                },
                SourceLocation::new(PathBuf::from("b"), 13..14)
            )]
        );
    }

    #[test]
    fn composed_path() {
        let errors = type_check_multi([
            (PathBuf::from("entry"), "use reef::foo::bar\nbar::here()"),
            (PathBuf::from("foo/bar"), "fun here() = {}"),
            (PathBuf::from("foo"), "use reef::foo::bar"),
        ]);
        assert_eq!(errors, []);
    }

    #[test]
    fn one_path_error() {
        let errors = type_check_multi([
            (PathBuf::from("test"), "reef::bar::test::foo()"),
            (PathBuf::from("bar"), "fun test() = {}"),
        ]);
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::UndefinedSymbol {
                    name: "test".to_owned(),
                    expected: SymbolRegistry::Type,
                    found: None,
                },
                SourceLocation::new(PathBuf::from("test"), 11..15),
            )]
        );
    }

    #[test]
    fn reuse_path() {
        let errors = type_check_multi([
            (PathBuf::from("test"), "use reef::bar::foo\nfoo::test()"),
            (PathBuf::from("bar/foo"), "fun test() = {}"),
        ]);
        assert_eq!(errors, []);
    }

    #[test]
    fn pass_types_across_files() {
        let errors = type_check_multi([
            (PathBuf::from("test"), "val x: Int = reef::bar::truthy()"),
            (PathBuf::from("bar"), "fun truthy() -> Bool = true"),
        ]);
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "Int".to_owned(),
                    expected_due_to: Some(SourceLocation::new(PathBuf::from("test"), 7..10)),
                    actual: "Bool".to_owned(),
                },
                SourceLocation::new(PathBuf::from("test"), 13..32),
            )]
        );
    }

    #[test]
    fn parameter_type_mismatch() {
        let errors = type_check_multi([
            (PathBuf::from("main"), "reef::lib::play(5, true)"),
            (PathBuf::from("lib"), "fun play(x: Bool, y: Int) = {}"),
        ]);
        assert_eq!(
            errors,
            [
                TypeError::new(
                    TypeErrorKind::TypeMismatch {
                        expected: "Bool".to_owned(),
                        expected_due_to: Some(SourceLocation::new(PathBuf::from("lib"), 9..16)),
                        actual: "Int".to_owned(),
                    },
                    SourceLocation::new(PathBuf::from("main"), 16..17),
                ),
                TypeError::new(
                    TypeErrorKind::TypeMismatch {
                        expected: "Int".to_owned(),
                        expected_due_to: Some(SourceLocation::new(PathBuf::from("lib"), 18..24)),
                        actual: "Bool".to_owned(),
                    },
                    SourceLocation::new(PathBuf::from("main"), 19..23),
                )
            ]
        );
    }

    #[test]
    #[ignore]
    fn invalid_binary_op() {
        let errors = type_check("'test' - 4");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "Int".to_owned(),
                    expected_due_to: Some(SourceLocation::new(PathBuf::from("main.msh"), 9..10)),
                    actual: "String".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 0..10),
            )]
        );
    }

    #[test]
    fn use_struct_attribute() {
        let errors = type_check("struct Bar { test: Int }; fun test(b: Bar) -> Int = $b.test");
        assert_eq!(errors, []);
    }

    #[test]
    fn use_unknown_struct_attribute() {
        let errors = type_check("struct Foo { bar: String }; fun test(b: Foo) -> String = $b.test");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::UnknownField {
                    name: "test".to_owned(),
                    type_name: "Foo".to_owned(),
                    available: vec!["bar".to_owned()]
                },
                SourceLocation::new(PathBuf::from("main.msh"), 60..64),
            )]
        );
    }

    #[test]
    fn generic_identity_function() {
        let errors = type_check("fun identity[T](x: T) -> T = $x\nval x: Int = identity(5)");
        assert_eq!(errors, []);
    }

    #[test]
    fn generic_explicit_function() {
        let errors = type_check("fun try[T](x: T); try::[String](5)");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "String".to_owned(),
                    expected_due_to: Some(SourceLocation::new(PathBuf::from("main.msh"), 11..15)),
                    actual: "Int".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 32..33),
            )]
        );
    }

    #[test]
    fn cannot_infer_generic() {
        let errors = type_check("fun bar[T]() -> T; bar()");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeAnnotationRequired {
                    types: vec!["T".to_owned()],
                    insert_at: 22,
                },
                SourceLocation::new(PathBuf::from("main.msh"), 19..24),
            )]
        );
    }

    #[test]
    fn simple_return() {
        let errors = type_check("fun foo() -> Int = true");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "Int".to_owned(),
                    expected_due_to: Some(SourceLocation::new(PathBuf::from("main.msh"), 13..16)),
                    actual: "Bool".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 19..23),
            )]
        );
    }

    #[test]
    fn block_return() {
        let errors = type_check(
            "fun foo() -> String = {
            if true { return 5 }
            return 'test'
        }",
        );
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "String".to_owned(),
                    expected_due_to: Some(SourceLocation::new(PathBuf::from("main.msh"), 13..19)),
                    actual: "Int".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 46..54),
            )]
        );
    }

    #[test]
    fn return_outside_function() {
        let errors = type_check("return");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::ReturnOutsideFunction,
                SourceLocation::new(PathBuf::from("main.msh"), 0..6),
            )]
        );
    }

    #[test]
    fn repeated_self_parameter() {
        let errors = type_check(
            "struct Foo {}
        impl Foo {
            fun foo(self, self) = {}
        }",
        );
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::RepeatedParameterName {
                    name: "self".to_owned(),
                    previous: 53..57,
                },
                SourceLocation::new(PathBuf::from("main.msh"), 59..63),
            )]
        );
    }

    #[test]
    fn access_self() {
        let errors = type_check(
            "struct Foo { bar: Int }
        impl Foo {
            fun get_bar(self) -> Int = $self.bar
        }",
        );
        assert_eq!(errors, []);
    }

    #[test]
    fn impl_template() {
        let errors = type_check(
            "struct Vec[T] {}
        impl[T] Vec[T] {
            fun get(self) -> T;
        }",
        );
        assert_eq!(errors, []);
    }

    #[test]
    fn impl_not_generic_struct() {
        let errors = type_check("struct Test {}\nimpl Test[Int] { fun play(); }");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::ArityMismatch {
                    expected: 0,
                    received: 1,
                },
                SourceLocation::new(PathBuf::from("main.msh"), 25..28),
            )]
        );
    }

    #[test]
    fn method_like_field() {
        let errors = type_check(
            "struct Bar {}\nimpl Bar { fun foo(self, count: Int); }\nfun take(bar: Bar) = $bar.foo",
        );
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::MethodLikeFieldAccess {
                    name: "foo".to_owned(),
                    parentheses: "(_)".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 80..83)
            )]
        );
    }

    #[test]
    fn accessing_generic_type() {
        let errors =
            type_check("struct Bar[T] { count: T }\nfun take(bar: Bar[Int]) -> Int = $bar.count");
        assert_eq!(errors, []);
    }

    #[test]
    fn accessing_indirect_generic_type() {
        let errors = type_check(
            "struct Vec[T] {}
            struct Foo[T] { vec: Vec[T] }
            fun take(foo: Foo[String]) -> Vec[String] = $foo.vec",
        );
        assert_eq!(errors, []);
    }

    #[test]
    fn incorrect_type_parameter_assign() {
        let errors = type_check("fun id[T]() -> T; val j: Int = id[String]()");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "Int".to_owned(),
                    expected_due_to: Some(SourceLocation::new(PathBuf::from("main.msh"), 25..28)),
                    actual: "String".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 31..43),
            )]
        );
    }

    #[test]
    fn magic_indirect_type() {
        let errors = type_check(
            "struct List[T] {}
            fun create[T]() -> List[T];
            val a: List[Int] = create()
            val b: List[Int] = create[String]()
            ",
        );
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "List[Int]".to_owned(),
                    expected_due_to: Some(SourceLocation::new(PathBuf::from("main.msh"), 117..126)),
                    actual: "List[String]".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 129..145),
            )]
        );
    }

    #[test]
    fn return_inference_last_resort() {
        let errors = type_check(
            "struct Box[T] {}
            fun create[T](t: T) -> Box[T];
            val a: Box[Unit] = create('hi')
            ",
        );
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "Box[Unit]".to_owned(),
                    expected_due_to: Some(SourceLocation::new(PathBuf::from("main.msh"), 79..88)),
                    actual: "Box[String]".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 91..103),
            )]
        );
    }

    #[test]
    fn inner_type_inference() {
        // Maybe point to the T or the other arg?
        let errors = type_check(
            "struct Box[T] {}
            fun box[T](content: T) -> Box[T];
            fun zip[T](a: Box[T], b: Box[T]) -> Box[T];
            zip(box('hi'), box(42))
            ",
        );
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "Box[String]".to_owned(),
                    expected_due_to: Some(SourceLocation::new(PathBuf::from("main.msh"), 97..106)),
                    actual: "Box[Int]".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 146..153),
            )]
        );
    }

    #[test]
    fn different_source_of_inference() {
        let errors = type_check(
            "struct Box[T] {}
            fun test[A, B](a: A) -> Box[B];
            val b: Box[Bool] = test(1);
            ",
        );
        assert_eq!(errors, []);
    }

    #[test]
    fn check_variable_multi_files() {
        let errors = type_check_multi([
            (
                PathBuf::from("main"),
                "use reef::other::letter\nval n: Int = $letter",
            ),
            (PathBuf::from("other"), "val letter = 'a'"),
        ]);
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "Int".to_owned(),
                    expected_due_to: Some(SourceLocation::new(PathBuf::from("main"), 31..34)),
                    actual: "String".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main"), 37..44),
            )]
        );
    }

    #[test]
    fn different_generic_len() {
        let errors = type_check("fun foo[T, U](t: T, u: U) -> T; foo::[Int](1, true)");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::ArityMismatch {
                    expected: 2,
                    received: 1,
                },
                SourceLocation::new(PathBuf::from("main.msh"), 38..41),
            )]
        );
    }

    #[test]
    fn implicit_exit_to_bool_var() {
        let errors = type_check("val res: Bool = { /bin/true }");
        assert_eq!(errors, []);
    }

    #[test]
    fn implicit_exit_to_bool_call() {
        let errors = type_check("fun test(b: Bool); test({ /bin/true })");
        assert_eq!(errors, []);
    }
}
