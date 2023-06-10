#[derive(Default)]
pub struct Bytecode {
    pub bytes: Vec<u8>,
    pub constants: Vec<String>,
}

impl Bytecode {
    pub fn emit_code(&mut self, code: Opcode) {
        self.bytes.push(code as u8);
    }

    pub fn emit_int_constant(&mut self, constant: i64) {
        self.emit_code(Opcode::PushInt);
        self.bytes.extend(constant.to_be_bytes());
    }

    pub fn emit_string_constant(&mut self, constant: String) {
        self.emit_code(Opcode::PushString);
        self.bytes.push(self.constants.len() as u8);
        self.constants.push(constant);
    }
}

#[repr(u8)]
pub enum Opcode {
    PushInt,
    PushFloat,
    PushString,
    GetLocal,
    SetLocal,
    Pop,
    Spawn,

    ConvertIntToStr,
    ConvertFloatToStr,
    ConvertIntToFloat,
}
