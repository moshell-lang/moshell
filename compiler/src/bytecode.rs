use std::mem::size_of;

use analyzer::typing::user::{self, TypeId};
use analyzer::typing::variable::{LocalId, Var};
use num_enum::TryFromPrimitive;

use crate::locals::LocalsLayout;
use crate::r#type::ValueStackSize;
use crate::structure::StructureLayout;

#[derive(Debug, Clone)]
pub struct Placeholder {
    pos: u32,
}

/// Holds the currently generated bytecode.
///
/// This struct provides support methods to emit bytecode primitives
/// such as ints, byte, double, constant references etc.
/// Also provides placeholder support for backpatching
#[derive(Default)]
pub struct Bytecode {
    pub(crate) bytes: Vec<u8>,
}

impl Bytecode {
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Returns the number of bytes
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// emits a signed 64 bits integer
    pub fn emit_int(&mut self, value: i64) {
        self.bytes.extend(value.to_be_bytes());
    }

    /// emits an unsigned byte
    pub fn emit_byte(&mut self, value: u8) {
        self.bytes.push(value);
    }

    /// emits an unsigned 32 bits integer
    pub fn emit_u32(&mut self, value: u32) {
        self.bytes.extend(value.to_be_bytes());
    }

    pub fn emit_i32(&mut self, value: i32) {
        self.bytes.extend(value.to_be_bytes());
    }

    /// emits a signed 64 bits float
    pub fn emit_float(&mut self, value: f64) {
        self.bytes.extend(value.to_be_bytes());
    }

    /// emits a constant pool reference, which is an unsigned 32 bits integer
    pub fn emit_constant_ref(&mut self, constant: u32) {
        self.emit_u32(constant);
    }

    /// Fills an instruction pointer at given instruction pointer in the byte array
    pub fn patch_u32_placeholder(&mut self, placeholder: Placeholder, value: u32) {
        let pos = placeholder.pos as usize;
        self.bytes[pos..pos + size_of::<u32>()].copy_from_slice(&value.to_be_bytes())
    }

    pub fn emit_instruction_pointer(&mut self, ip: usize) {
        self.bytes.extend(ip.to_be_bytes());
    }

    /// expands the byte vector to let a placeholder of the given size,
    /// returning the position of the placeholder in the vector
    pub fn emit_u32_placeholder(&mut self) -> Placeholder {
        let pos = self.bytes.len();
        self.bytes.resize(pos + size_of::<u32>(), 0);
        Placeholder { pos: pos as u32 }
    }
}

/// binds a source code byte position to a bytecode instruction index
pub struct InstructionPos {
    pub source_code_byte_pos: usize,
    pub instruction: u32,
}

/// This structure is a wrapper around [`Bytecode`] and is used
/// to emit bytecode instructions.
pub struct Instructions<'a> {
    bytecode: &'a mut Bytecode,
    /// Offset of where this instruction set starts in the given bytecode
    ip_offset: u32,

    /// Starting byte position in the source code for some instruction pointers
    positions: Vec<InstructionPos>,
}

impl<'a> Instructions<'a> {
    pub fn wrap(bytecode: &'a mut Bytecode) -> Self {
        Self {
            ip_offset: bytecode.len() as u32,
            bytecode,
            positions: Vec::new(),
        }
    }

    /// Emits instructions to instantiate a new structure,
    /// where the given u32 is the constant pool index of the fqn string identifier of the structure to instance
    pub fn emit_new(&mut self, structure_fqn: u32) {
        self.emit_code(Opcode::NewStruct);
        self.bytecode.emit_constant_ref(structure_fqn);
    }

    pub fn emit_code(&mut self, code: Opcode) {
        self.bytecode.emit_byte(code as u8)
    }

    pub fn emit_copy_operands(&mut self, count: u32) {
        self.emit_code(Opcode::StructCopyOperands);
        self.bytecode.emit_u32(count);
    }

    pub fn emit_pop(&mut self, size: ValueStackSize) {
        let pop_opcode = match size {
            ValueStackSize::Zero => return,
            ValueStackSize::Byte => Opcode::PopByte,
            ValueStackSize::QWord => Opcode::PopQWord,
        };
        self.emit_code(pop_opcode);
    }

    pub fn emit_box_if_primitive(&mut self, ty: TypeId) {
        match ty {
            user::BOOL_TYPE => self.emit_code(Opcode::BoxByte),
            user::INT_TYPE | user::FLOAT_TYPE => self.emit_code(Opcode::BoxQWord),
            _ => { /* Objects are already on the heap */ }
        }
    }

    /// bind a source code byte position to current instruction
    pub fn push_position(&mut self, pos: usize) {
        self.positions.push(InstructionPos {
            source_code_byte_pos: pos,
            instruction: self.current_ip(),
        })
    }

    pub fn take_positions(&mut self) -> Vec<InstructionPos> {
        std::mem::take(&mut self.positions)
    }

    /// emits instructions to assign given local identifier with last operand stack value
    /// assuming the value size is the given `size` argument
    pub fn emit_set_local(&mut self, var: LocalId, size: ValueStackSize, layout: &LocalsLayout) {
        let index = layout.get_index(var);
        let opcode = match size {
            ValueStackSize::Byte => Opcode::SetLocalByte,
            ValueStackSize::QWord => Opcode::SetLocalQWord,
            ValueStackSize::Zero => return,
        };
        self.emit_code(opcode);
        self.bytecode.emit_u32(index);
    }

    pub fn emit_set_capture(
        &mut self,
        capture: LocalId,
        size: ValueStackSize,
        layout: &LocalsLayout,
    ) {
        let index = layout.get_capture_index(capture).unwrap();
        // get capture reference
        self.emit_code(Opcode::GetLocalQWord);
        self.bytecode.emit_u32(index);
        // reference is now on operand stack

        let opcode = match size {
            ValueStackSize::Byte => Opcode::SetRefByte,
            ValueStackSize::QWord => Opcode::SetRefQWord,
            ValueStackSize::Zero => return,
        };
        self.emit_code(opcode);
    }

    /// Emits the instructions to assign the value on top of the stack to the given external symbol.
    pub fn emit_set_external(&mut self, symbol_id: u32, size: ValueStackSize) {
        self.emit_code(match size {
            ValueStackSize::Byte => Opcode::StoreByte,
            ValueStackSize::QWord => Opcode::StoreQWord,
            ValueStackSize::Zero => return,
        });
        self.bytecode.emit_constant_ref(symbol_id);
    }

    /// emits instructions to push to operand stack given local identifier
    /// assuming the local's size is the given `size` argument
    pub fn emit_get_local(&mut self, local: LocalId, size: ValueStackSize, layout: &LocalsLayout) {
        let index = layout.get_index(local);
        let opcode = match size {
            ValueStackSize::Byte => Opcode::GetLocalByte,
            ValueStackSize::QWord => Opcode::GetLocalQWord,
            ValueStackSize::Zero => return,
        };
        self.emit_code(opcode);
        self.bytecode.emit_u32(index);
    }

    pub fn emit_get_capture(
        &mut self,
        capture: LocalId,
        size: ValueStackSize,
        layout: &LocalsLayout,
    ) {
        let index = layout.get_capture_index(capture).unwrap();
        // get capture reference
        self.emit_code(Opcode::GetLocalQWord);
        self.bytecode.emit_u32(index);
        // reference is now on operand stack

        let opcode = match size {
            ValueStackSize::Byte => Opcode::GetRefByte,
            ValueStackSize::QWord => Opcode::GetRefQWord,
            ValueStackSize::Zero => {
                panic!("get_capture on captured variable which is zero-sized")
            }
        };
        self.emit_code(opcode);
    }

    /// pushes a reference to the given symbol on the stack's locals
    pub fn emit_push_stack_ref(&mut self, var: Var, layout: &LocalsLayout) {
        todo!("emit_push_stack_ref")
    }

    /// Emits the instructions to push the value of the given external symbol on top of the stack.
    pub fn emit_get_external(&mut self, symbol_id: u32, size: ValueStackSize) {
        self.emit_code(match size {
            ValueStackSize::Byte => Opcode::FetchByte,
            ValueStackSize::QWord => Opcode::FetchQWord,
            ValueStackSize::Zero => return,
        });
        self.bytecode.emit_constant_ref(symbol_id);
    }

    pub fn emit_get_field(&mut self, field_id: LocalId, layout: &StructureLayout) {
        let (field_index, size) = layout.get_emplacement(field_id);
        self.emit_code(match size {
            ValueStackSize::Byte => Opcode::GetStructByte,
            ValueStackSize::QWord => Opcode::GetStructQWord,
            ValueStackSize::Zero => return,
        });
        self.bytecode.emit_u32(field_index);
    }

    pub fn emit_set_field(&mut self, field_id: LocalId, layout: &StructureLayout) {
        let (field_index, size) = layout.get_emplacement(field_id);
        self.emit_code(match size {
            ValueStackSize::Byte => Opcode::SetStructByte,
            ValueStackSize::QWord => Opcode::SetStructQWord,
            ValueStackSize::Zero => panic!("set for value whose type is zero-sized"),
        });
        self.bytecode.emit_u32(field_index);
    }

    /// emits instructions to push an integer in the operand stack
    pub fn emit_push_int(&mut self, constant: i64) {
        self.emit_code(Opcode::PushInt);
        self.bytecode.emit_int(constant);
    }

    /// emits instructions to push an unsigned byte in the operand stack
    pub fn emit_push_byte(&mut self, constant: u8) {
        self.emit_code(Opcode::PushByte);
        self.bytecode.emit_byte(constant);
    }

    /// emits instructions to push a float in the operand stack
    pub fn emit_push_float(&mut self, constant: f64) {
        self.emit_code(Opcode::PushFloat);
        self.bytecode.emit_float(constant)
    }

    /// emits instructions to push a pool reference in the operand stack
    pub fn emit_push_constant_ref(&mut self, constant_ref: u32) {
        self.emit_code(Opcode::PushStringRef);
        self.bytecode.emit_constant_ref(constant_ref)
    }

    /// Inverts the boolean value on top of the stack.
    pub fn emit_bool_inversion(&mut self) {
        self.emit_push_byte(1);
        self.emit_code(Opcode::BXor);
    }

    /// emits an instruction pointer
    fn emit_instruction_pointer(&mut self, ip: u32) {
        self.bytecode.emit_u32(ip);
    }

    /// Emits a jump instruction.
    ///
    /// It returns the [`Placeholder`] address of the offset which is to be patched.
    #[must_use = "the jump address must be patched later"]
    pub fn emit_jump(&mut self, opcode: Opcode) -> Placeholder {
        debug_assert!(
            matches!(
                opcode,
                Opcode::Jump | Opcode::IfJump | Opcode::IfNotJump | Opcode::Fork
            ),
            "input opcode must be a jump instruction"
        );
        self.emit_code(opcode);
        self.bytecode.emit_u32_placeholder()
    }

    pub fn emit_open(&mut self, unix_flags: i32) {
        self.emit_code(Opcode::Open);
        self.bytecode.emit_i32(unix_flags);
    }

    /// Emits a function invocation instruction, with a given method signature in constant pool.
    pub fn emit_invoke(&mut self, signature_idx: u32) {
        self.emit_code(Opcode::Invoke);
        self.bytecode.emit_constant_ref(signature_idx);
    }

    /// Takes the index of the jump offset to be patched as input, and patches
    /// it to point to the current instruction.
    pub fn patch_jump(&mut self, offset_idx: Placeholder) {
        let ip = self.current_ip();
        self.bytecode.patch_u32_placeholder(offset_idx, ip);
    }

    /// Emits a jump instruction to the given instruction pointer.
    pub fn jump_back_to(&mut self, start_idx: u32) {
        self.emit_code(Opcode::Jump);
        self.emit_instruction_pointer(start_idx);
    }

    /// Returns the current instruction pointer
    pub fn current_ip(&self) -> u32 {
        u32::try_from(self.bytecode.len()).expect("Too much bytecode") - self.ip_offset
    }
}

/// see vm's `Opcode` enum for more details
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
pub enum Opcode {
    PushInt,
    PushByte,
    PushFloat,
    PushStringRef,
    PushLocalRef,

    BoxQWord,
    BoxByte,
    Unbox,

    GetLocalByte,
    SetLocalByte,
    GetLocalQWord,
    SetLocalQWord,

    GetRefByte,
    SetRefByte,
    GetRefQWord,
    SetRefQWord,

    GetStructByte,
    SetStructByte,
    GetStructQWord,
    SetStructQWord,

    FetchByte,
    FetchQWord,
    StoreByte,
    StoreQWord,

    NewStruct,
    StructCopyOperands,

    Invoke,
    Fork,
    Exec,
    Wait,
    Open,
    Close,
    SetupRedirect,
    Redirect,
    PopRedirect,
    Pipe,
    Read,
    Write,
    Exit,

    Dup,
    DupByte,
    Swap,
    Swap2,
    PopByte,
    PopQWord,

    IfJump,
    IfNotJump,
    Jump,

    Return,

    ConvertByteToInt,
    ConvertIntToByte,

    BXor,
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntMod,
    IntNeg,
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatNeg,

    IntEqual,
    IntLessThan,
    IntLessOrEqual,
    IntGreaterThan,
    IntGreaterOrEqual,
    FloatEqual,
    FloatLessThan,
    FloatLessOrEqual,
    FloatGreaterThan,
    FloatGreaterOrEqual,
}
