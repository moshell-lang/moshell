use crate::bytecode::Opcode::*;
use enum_assoc::Assoc;
use std::mem::size_of;
#[derive(Default)]
pub struct Bytecode {
    pub bytes: Vec<u8>,
}

impl Bytecode {
    /// Returns the current byte instruction position in current frame
    pub fn current_frame_ip(&self) -> usize {
        // note: as call frames are not yet supported,
        // this implementation returns the overall bytes length.
        self.bytes.len()
    }

    pub fn emit_code(&mut self, code: Opcode) {
        self.bytes.push(code as u8);
    }

    pub fn emit_get_local(&mut self, identifier: u8) {
        self.emit_code(Opcode::GetLocal);
        self.bytes.push(identifier);
    }

    pub fn emit_int(&mut self, constant: i64) {
        self.emit_code(Opcode::PushInt);
        self.bytes.extend(constant.to_be_bytes());
    }

    pub fn emit_float(&mut self, constant: f64) {
        self.emit_code(Opcode::PushFloat);
        self.bytes.extend(constant.to_be_bytes());
    }

    pub fn emit_string_constant_ref(&mut self, constant_ref: usize) {
        self.emit_code(Opcode::PushString);
        self.bytes.extend(constant_ref.to_be_bytes());
    }

    pub fn emit_instruction_pointer(&mut self, ip: usize) {
        self.bytes.extend(ip.to_be_bytes());
    }

    /// expands the byte vector to let a placeholder of the given size,
    /// returning the position of the placeholder in the vector
    pub fn create_placeholder(&mut self, placeholder_size: usize) -> usize {
        let pos = self.bytes.len();
        self.bytes.resize(pos + placeholder_size, 0);
        pos
    }

    pub fn extend(&mut self, bytecode: Bytecode) {
        self.bytes.extend(bytecode.bytes);
    }

    /// Fills an instruction pointer at given instruction pointer in the byte array
    pub fn fill_in_ip(&mut self, ip_dest: usize, ip: usize) {
        self.bytes[ip_dest..ip_dest + size_of::<usize>()]
            .copy_from_slice(ip.to_be_bytes()[..].try_into().unwrap())
    }
}

#[repr(u8)]
#[derive(Assoc)]
#[func(pub fn mnemonic(&self) -> &'static str)]
pub enum Opcode {
    #[assoc(mnemonic = "pshi")]
    PushInt,
    #[assoc(mnemonic = "pshf")]
    PushFloat,
    #[assoc(mnemonic = "pshs")]
    PushString,
    #[assoc(mnemonic = "get")]
    GetLocal,
    #[assoc(mnemonic = "set")]
    SetLocal,
    #[assoc(mnemonic = "spawn")]
    Spawn,

    #[assoc(mnemonic = "pop")]
    PopByte,
    #[assoc(mnemonic = "pop8")]
    PopQWord,

    #[assoc(mnemonic = "ifjmp")]
    IfJump,
    #[assoc(mnemonic = "ifnjmp")]
    IfNotJump,
    #[assoc(mnemonic = "jmp")]
    Jump,

    #[assoc(mnemonic = "i2s")]
    ConvertIntToStr,
    #[assoc(mnemonic = "f2s")]
    ConvertFloatToStr,
}

impl TryFrom<u8> for Opcode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let opcode = match value {
            0 => PushInt,
            1 => PushFloat,
            2 => PushString,
            3 => GetLocal,
            4 => SetLocal,
            5 => Spawn,

            6 => PopByte,
            7 => PopQWord,

            8 => IfJump,
            9 => IfNotJump,
            10 => Jump,

            11 => ConvertIntToStr,
            12 => ConvertFloatToStr,
            _ => return Err(()),
        };
        Ok(opcode)
    }
}
