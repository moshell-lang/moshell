use crate::engine::Engine;
use analyzer_system::environment::Environment;
use analyzer_system::name::Name;
use analyzer_system::resolver::{ResolvedSymbol, Resolver, SourceObjectId};
use ast::r#use::Import;
use ast::Expr;
use std::collections::HashMap;
use std::iter::once;

#[derive(Default)]
struct Imports {
    imported_symbols: HashMap<String, ResolvedSymbol>,
}

impl Imports {
    fn set_import(&mut self, symbol_name: String, symbol: ResolvedSymbol) {
        self.imported_symbols.insert(symbol_name, symbol);
    }
}

fn resolve_globals(engine: &Engine, _resolver: &mut Resolver) -> Result<(), String> {
    for (ast, _env) in &engine.origins {
        let _imports = resolve_imports(ast, engine)?;
    }
    Ok(())
}

fn resolve_import(
    import: &Import,
    relative_path: Vec<String>,
    imports: &mut Imports,
    engine: &Engine,
) -> Result<(), String> {
    fn get_env_from<'a>(
        relative: Vec<String>,
        path: &Vec<&str>,
        engine: &'a Engine,
    ) -> Result<(SourceObjectId, &'a Environment), String> {
        let mut path_vec = relative;
        path_vec.extend(path.iter().map(|s| s.to_string()).collect::<Vec<_>>());

        let env_name = Name::from(path_vec);

        let env_id = engine.find_environment_by_name(&env_name);
        let env_id = env_id.ok_or_else(|| format!("Unknown module {env_name}"))?;
        Ok((env_id, engine.find_environment(env_id).unwrap()))
    }

    match import {
        Import::Symbol(s) => {
            let (env_id, env) = get_env_from(relative_path, &s.path, engine)?;
            let symbol_name = s.name;

            let symbol_id = env
                .variables
                .position_exported_var(s.name)
                .ok_or_else(|| format!("unknown symbol {symbol_name} in module {}", env.fqn))?;

            let import_symbol = ResolvedSymbol::new(env_id, symbol_id);
            let key_name = s.alias.unwrap_or_else(|| s.name).to_owned();

            imports.set_import(key_name, import_symbol);
            Ok(())
        }
        Import::AllIn(path, _) => {
            let (env_id, env) = get_env_from(relative_path, &path, engine)?;

            for var in env.variables.list_exported_vars() {
                let var_id = env.variables.position_exported_var(&var.name).unwrap();

                let import_symbol = ResolvedSymbol::new(env_id, var_id);
                imports.set_import(var.name.clone(), import_symbol);
            }

            Ok(())
        }

        Import::Environment(_, _) => {
            Err("import of environment variables and commands are not yet supported.".to_owned())
        }
        Import::List(list) => {
            for list_import in &list.imports {
                //append ImportList's path to current relative path
                let mut relative = relative_path.clone();
                relative.extend(list.path.iter().map(|s| s.to_string()).collect::<Vec<_>>());

                resolve_import(&list_import, relative, imports, engine)?
            }
            Ok(())
        }
    }
}

fn resolve_imports(ast: &Expr, engine: &Engine) -> Result<Imports, String> {
    fn resolve_imports_exprs<'a>(
        exprs: impl Iterator<Item = &'a Expr<'a>>,
        engine: &Engine,
    ) -> Result<Imports, String> {
        let mut imports = Imports::default();
        let mut met_non_use_expr = false;
        for expr in exprs {
            if let Expr::Use(u) = expr {
                if met_non_use_expr {
                    return Err("use statement within expressions".to_owned());
                }
                resolve_import(&u.import, Vec::new(), &mut imports, engine)?;
                continue;
            }
            met_non_use_expr = true
        }
        Ok(imports)
    }
    match ast {
        Expr::Block(b) => resolve_imports_exprs(b.expressions.iter(), engine),
        other => resolve_imports_exprs(once(other), engine),
    }
}


#[cfg(test)]
mod tests {
    use analyzer_system::name::Name;
    use analyzer_system::resolver::Resolver;
    use context::source::Source;
    use crate::engine::Engine;
    use crate::import::CachedImporter;
    use crate::steps::collect::collect_symbols;
    use crate::steps::resolve::resolve_imports;

    #[test]
    #[ignore]
    fn test_imports_resolution() {

        let math_ast = Source::unknown("val PI = 3.14");

        let std_ast = Source::unknown("val Foo = 'moshell_std'; val Bar = $Foo");

        let io_ast = Source::unknown("val out = OutputStream(); val in = InputStream()");

        let test_ast = Source::unknown("\
            use math::PI
            use std::{Bar, io::*}
        ");

        let mut engine = Engine::default();
        let mut resolver = Resolver::default();
        let mut importer = CachedImporter::new([
            (Name::new("math"), math_ast),
            (Name::new("std"), std_ast),
            (Name::new("std::io"), io_ast),
            (Name::new("test"), test_ast),
        ]);
        collect_symbols(&mut engine,
                        &mut resolver,
                        Name::new("test"),
                        &mut importer).expect("collect errors");

        resolve_imports(engine.origins[0].0, &engine).expect("resolution errors");
    }
}