use analyzer_system::environment::Environment;
use analyzer_system::name::Name;
use analyzer_system::resolver::{GlobalObjectId, Resolver};
use ast::Expr;

#[derive(Debug, Clone, Copy)]
struct ResolutionState {
    module: GlobalObjectId,
}

fn first_pass(expr: &Expr) {
    let mut resolver = Resolver::default();
    let state = ResolutionState {
        module: resolver.track_new_object(),
    };
    tree_walk(&mut resolver, &mut Environment::named(Name::new("a")), state, expr);
}

fn tree_walk(resolver: &mut Resolver, env: &mut Environment, state: ResolutionState, expr: &Expr) {
    match expr {
        Expr::VarDeclaration(var) => {
            env.variables.declare_local(var.var.name.to_owned());
        },
        Expr::VarReference(var) => {
            env.variables.identify(resolver, var.name);
        },
        Expr::Block(block) => {
            env.begin_scope();
            for expr in &block.expressions {
                tree_walk(resolver, env, state, expr);
            }
            env.end_scope();
        },
        Expr::If(if_expr) => {
            env.begin_scope();
            tree_walk(resolver, env, state, &if_expr.condition);
            env.end_scope();
            env.begin_scope();
            tree_walk(resolver, env, state, &if_expr.success_branch);
            env.end_scope();
            if let Some(else_branch) = &if_expr.fail_branch {
                env.begin_scope();
                tree_walk(resolver, env, state, else_branch);
                env.end_scope();
            }
        },
        _ => todo!("first pass for {:?}", expr)
    }
}

#[cfg(test)]
mod tests {
    use ast::group::Block;
    use ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
    use super::*;

    #[test]
    fn bind_local_variables() {
        let expr = Expr::Block(Block {
            expressions: vec![
                Expr::VarDeclaration(VarDeclaration {
                    kind: VarKind::Var,
                    var: TypedVariable {
                        name: "bar",
                        ty: None,
                        segment: 0..1,
                    },
                    initializer: None,
                    segment: 0..2,
                }),
                Expr::VarReference(VarReference {
                    name: "bar",
                    segment: 0..1,
                }),
            ],
            segment: 0..3
        });
        first_pass(&expr);
    }
}
