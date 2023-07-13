use compiler::bytecode::Opcode;
use std::io;
use std::io::{Cursor, Read};

macro_rules! read {
    ($read:expr, $tpe:ty) => {{
        let mut bytes: [u8; std::mem::size_of::<$tpe>()] = [0; std::mem::size_of::<$tpe>()];
        $read.read_exact(&mut bytes)?;
        <$tpe>::from_be_bytes(bytes)
    }};
}

/// counts the number of digits of an u64 in base ten
fn digits(val: u64) -> usize {
    val.checked_ilog10().unwrap_or(0) as usize + 1
}

fn load_constants(reader: &mut impl Read) -> io::Result<Vec<String>> {
    let mut constants = Vec::new();
    let pool_length = read!(reader, u32);

    for _ in 0..pool_length {
        let str_len = read!(reader, u64) as usize;

        let mut buff = vec![0; str_len];
        reader.read_exact(&mut buff)?;

        let str = String::from_utf8(buff).expect("read String is not utf8");
        constants.push(str);
    }

    Ok(constants)
}

fn display_constants(constants: &[String]) {
    println!("Constant Pool: ");
    let idx_pan = digits(constants.len() as u64);
    for (idx, str) in constants.iter().enumerate() {
        println!("#{idx:<idx_pan$}: \"{str}\"");
    }
}

fn display_function(cursor: &mut Cursor<&[u8]>, constants: &[String]) -> io::Result<()> {
    let name = &constants[read!(cursor, u32) as usize];

    let locals_byte_count = read!(cursor, u32);
    let parameters_bytes_count = read!(cursor, u32);
    let return_bytes_count = read!(cursor, u8);

    let instruction_count = read!(cursor, u32);

    let instruction_pad = digits(instruction_count as u64);

    println!("{name}:");
    println!("\tlocals      : {locals_byte_count} bytes (including {parameters_bytes_count} bytes used for parameters)");
    println!("\treturn      : {return_bytes_count} bytes");
    println!("\tinstructions: {instruction_count} bytes");

    let start_pos = cursor.position();
    let end_pos = start_pos + instruction_count as u64;

    while cursor.position() < end_pos {
        let instruction_address = cursor.position() - start_pos;
        let opcode = Opcode::try_from(read!(cursor, u8)).expect("Unknown opcode");
        let mnemonic = get_opcode_mnemonic(opcode);
        print!("\t\t#{instruction_address:<instruction_pad$}: {mnemonic:7} ");

        match opcode {
            Opcode::PushInt => print!("<value {}>", read!(cursor, i64)),
            Opcode::PushByte => print!("<value {}>", read!(cursor, u8)),
            Opcode::PushFloat => print!("<value {}>", read!(cursor, f64)),
            Opcode::PushString => {
                let constant_idx = read!(cursor, u32) as usize;
                let str = &constants[constant_idx];
                let padding = (digits(constants.len() as u64) - digits(constant_idx as u64)) + 10;
                print!("<constant #{constant_idx}> {:padding$} // \"{str}\"", "")
            }
            Opcode::Invoke => {
                let constant_idx = read!(cursor, u32) as usize;
                let str = &constants[constant_idx];
                let padding = (digits(constants.len() as u64) - digits(constant_idx as u64)) + 10;
                print!("<function #{constant_idx}> {:padding$} // {str}", "")
            }
            Opcode::GetByte
            | Opcode::SetByte
            | Opcode::GetQWord
            | Opcode::SetQWord
            | Opcode::GetRef
            | Opcode::SetRef => {
                print!("<local @{}>", read!(cursor, u32))
            }
            Opcode::Spawn => print!("<arity {}>", read!(cursor, u8)),
            Opcode::IfJump | Opcode::IfNotJump | Opcode::Jump => {
                print!("<instruction #{}>", read!(cursor, u32))
            }
            _ => {} // Other opcodes do not define parameters
        }
        println!()
    }

    Ok(())
}

fn display_functions(cursor: &mut Cursor<&[u8]>, constants: &[String]) -> io::Result<()> {
    println!("Functions: ");

    let function_count = read!(cursor, u32);
    for _ in 0..function_count {
        display_function(cursor, constants)?;
    }
    Ok(())
}

pub(crate) fn display_bytecode(bytecode: &[u8]) {
    let mut cursor = Cursor::new(bytecode);
    let constants =
        load_constants(&mut cursor).expect("Read slice error when displaying constant pool");
    display_constants(&constants);
    display_functions(&mut cursor, &constants)
        .expect("Read slice error when displaying function contents");
}

fn get_opcode_mnemonic(opcode: Opcode) -> &'static str {
    match opcode {
        Opcode::PushInt => "ipsh",
        Opcode::PushByte => "ipsh",
        Opcode::PushFloat => "fpsh",
        Opcode::PushString => "spsh",
        Opcode::GetByte => "bget",
        Opcode::SetByte => "sset",
        Opcode::GetQWord => "qwget",
        Opcode::SetQWord => "qwset",
        Opcode::GetRef => "rget",
        Opcode::SetRef => "rset",
        Opcode::Spawn => "spawn",
        Opcode::Invoke => "invoke",
        Opcode::PopByte => "bpop",
        Opcode::PopQWord => "qwpop",
        Opcode::PopRef => "rpop",
        Opcode::IfJump => "ifjmp",
        Opcode::IfNotJump => "ifnjmp",
        Opcode::Jump => "jmp",
        Opcode::Return => "ret",
        Opcode::ConvertByteToInt => "b2i",
        Opcode::ConvertIntToStr => "i2s",
        Opcode::ConvertFloatToStr => "f2s",
        Opcode::ConvertIntToByte => "i2b",
        Opcode::Concat => "concat",
        Opcode::BXor => "bxor",
        Opcode::IntAdd => "iadd",
        Opcode::IntSub => "isub",
        Opcode::IntMul => "imul",
        Opcode::IntDiv => "idiv",
        Opcode::IntMod => "imod",
        Opcode::FloatAdd => "fadd",
        Opcode::FloatSub => "fsub",
        Opcode::FloatMul => "fmul",
        Opcode::FloatDiv => "fdiv",
        Opcode::StringEqual => "streq",
        Opcode::IntEqual => "ieq",
        Opcode::IntLessThan => "ilt",
        Opcode::IntLessOrEqual => "ile",
        Opcode::IntGreaterThan => "igt",
        Opcode::IntGreaterOrEqual => "ige",
        Opcode::FloatEqual => "feq",
        Opcode::FloatLessThan => "flt",
        Opcode::FloatLessOrEqual => "fle",
        Opcode::FloatGreaterThan => "fgt",
        Opcode::FloatGreaterOrEqual => "fge",
    }
}
