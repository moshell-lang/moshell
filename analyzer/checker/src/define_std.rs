use std::cell::RefCell;
use std::rc::Rc;

use analyzer_system::lang_types::any;
use analyzer_system::layers::ModuleLayers;
use analyzer_system::name::Name;
use analyzer_system::types::class::ClassTypeDefinition;
use analyzer_system::types::context::TypeContext;
use analyzer_system::types::types::Type;

pub(crate) fn define_ast(layers: &Rc<RefCell<ModuleLayers>>) -> Result<(), String> {
    let std = ModuleLayers::declare_env(layers, &Name::new("std"))?;

    let std = std.borrow();
    let std_ctx = &std.type_context;

    //Define the Iterable[A]: Any type
    let iterable = TypeContext::define_class(std_ctx,
                                             ClassTypeDefinition::new(Name::new("Iterable"))
                                                 .with_generic("A", any()),
    )?;

    //Define the List[A]: Iterable[A] type
    TypeContext::define_class(std_ctx,
                              ClassTypeDefinition::new(Name::new("List"))
                                  .with_super(iterable.clone())
                                  .with_generic("A", any())
                                  .with_association(0, Type::cons("List::A")),
    )?;

    //Define the Option[A]: Iterable[A] type
    let option = TypeContext::define_class(std_ctx,
                                           ClassTypeDefinition::new(Name::new("Option"))
                                               .with_super(iterable.clone())
                                               .with_generic("A", any())
                                               .with_association(0, Type::cons("Option::A")),
    )?;

    //Define the Some[A]: Option[A] type
    TypeContext::define_class(std_ctx,
                              ClassTypeDefinition::new(Name::new("Some"))
                                  .with_super(option.clone())
                                  .with_generic("A", any())
                                  .with_association(0, Type::cons("Some::A")),
    )?;

    //Define the None: Option[None] type
    TypeContext::define_class(std_ctx,
                              ClassTypeDefinition::new(Name::new("None"))
                                  .with_super(option.clone())
                                  .with_association(0, Type::Nothing),
    )?;


    //Define the Try[A, E] type
    let try_cl = TypeContext::define_class(std_ctx,
                                           ClassTypeDefinition::new(Name::new("Try"))
                                               .with_generic("A", any())
                                               .with_generic("E", any()),
    )?;

    //Define the Ok[A]: Try[A, Nothing] type
    TypeContext::define_class(std_ctx,
                              ClassTypeDefinition::new(Name::new("Ok"))
                                  .with_super(try_cl.clone())
                                  .with_generic("A", any())
                                  .with_association(0, Type::cons("Ok::A"))
                                  .with_association(1, Type::Nothing),
    )?;

    //Define the Err[E]: Try[Nothing, E] type
    TypeContext::define_class(std_ctx,
                              ClassTypeDefinition::new(Name::new("Err"))
                                  .with_super(try_cl.clone())
                                  .with_generic("E", any())
                                  .with_association(0, Type::Nothing)
                                  .with_association(1, Type::cons("Err::E")),
    )?;

    Ok(())
}

