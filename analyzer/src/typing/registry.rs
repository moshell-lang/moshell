use crate::typing::function::Function;
use crate::typing::schema::Schema;
use crate::typing::user::GENERIC_TYPE;
use std::ops::{Index, IndexMut};

#[derive(Clone)]
pub struct Registry {
    schemas: Vec<Schema>,
    functions: Vec<Function>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SchemaId(usize);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct FunctionId(usize);

impl Default for Registry {
    fn default() -> Self {
        Self {
            schemas: vec![
                Schema::new("Int".to_owned()),
                Schema::new("Bool".to_owned()),
                Schema::new("Exitcode".to_owned()),
                Schema::new("Float".to_owned()),
                Schema::new("String".to_owned()),
                Schema::generic("Vec".to_owned(), vec![GENERIC_TYPE]),
                Schema::new("Glob".to_owned()),
                Schema::new("Pid".to_owned()),
            ],
            functions: Vec::new(),
        }
    }
}

pub const INT_SCHEMA: SchemaId = SchemaId(0);
pub const BOOL_SCHEMA: SchemaId = SchemaId(1);
pub const EXITCODE_SCHEMA: SchemaId = SchemaId(2);
pub const FLOAT_SCHEMA: SchemaId = SchemaId(3);
pub const STRING_SCHEMA: SchemaId = SchemaId(4);
pub const VEC_SCHEMA: SchemaId = SchemaId(5);
pub const GLOB_SCHEMA: SchemaId = SchemaId(6);
pub const PID_SCHEMA: SchemaId = SchemaId(7);

impl Registry {
    /// Allocates a new [`SchemaId`] for the given [`Schema`].
    pub(crate) fn define_schema(&mut self, schema: Schema) -> SchemaId {
        let id = self.schemas.len();
        self.schemas.push(schema);
        SchemaId(id)
    }

    /// Allocates a new [`FunctionId`] for the given [`Function`].
    pub(crate) fn define_function(&mut self, function: Function) -> FunctionId {
        let id = self.functions.len();
        self.functions.push(function);
        FunctionId(id)
    }
}

macro_rules! impl_index {
    ($id:ty, $output:ty, $field:ident) => {
        impl Index<$id> for Registry {
            type Output = $output;

            fn index(&self, index: $id) -> &Self::Output {
                &self.$field[index.0]
            }
        }

        impl IndexMut<$id> for Registry {
            fn index_mut(&mut self, index: $id) -> &mut Self::Output {
                &mut self.$field[index.0]
            }
        }

        impl $id {
            pub fn get(self) -> usize {
                self.0
            }
        }
    };
}

impl_index!(SchemaId, Schema, schemas);
impl_index!(FunctionId, Function, functions);
