use crate::parser::Parser;

pub(crate) trait TestAspect {

}

impl<'a> TestAspect for Parser<'a> {

}