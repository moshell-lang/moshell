use crate::environment::symbols::MagicSymbolKind;
use crate::name::Name;
use crate::steps::typing::exploration::{Exploration, Links};
use crate::types::hir::{Declaration, ExprKind, FunctionCall, TypedExpr};
use crate::types::ty::Type;
use crate::types::{builtin, UNIT};
use context::source::{SourceSegment, SourceSegmentHolder};
use std::str::FromStr;

/// Checks if the given name is reserved for an external variable that is not
/// defined in the source.
pub fn is_magic_variable_name(name: &str) -> bool {
    u32::from_str(name).is_ok() || matches!(name, "*" | "@" | "#")
}

pub(super) fn prepend_implicits(
    body: TypedExpr,
    exploration: &Exploration,
    links: Links,
) -> TypedExpr {
    if let Some(id) = links
        .env()
        .symbols
        .find_magic(MagicSymbolKind::ProgramArguments)
    {
        let (std_reef, get_args_function) = exploration
            .externals
            .get_reef_by_name("std")
            .and_then(|(r, reef_id)| {
                r.engine
                    .find_environment_by_name(&Name::new("std::memory::program_arguments"))
                    .zip(Some(reef_id))
            })
            .map(|((id, _), reef_id)| (reef_id, id))
            .expect("could not find `std::memory::program_arguments` function");

        let get_args_chunk = exploration.get_chunk(std_reef, get_args_function).unwrap();
        let Type::Function(_, get_args_function_id) =
            exploration.typing.get_type(get_args_chunk.tpe).unwrap()
        else {
            unreachable!()
        };

        let generated_pargs_expr = TypedExpr {
            kind: ExprKind::Declare(Declaration {
                identifier: id,
                value: Some(Box::new(TypedExpr {
                    kind: ExprKind::FunctionCall(FunctionCall {
                        arguments: vec![],
                        reef: std_reef,
                        function_id: *get_args_function_id,
                        source_id: Some(get_args_function),
                    }),
                    ty: builtin::STRING_VEC,
                    segment: Default::default(),
                })),
            }),
            ty: UNIT,
            segment: SourceSegment::default(),
        };

        TypedExpr {
            ty: body.ty,
            segment: body.segment(),
            kind: ExprKind::Block(vec![generated_pargs_expr, body]),
        }
    } else {
        body
    }
}
