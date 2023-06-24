use compiler::bytecode::Opcode;

pub(crate) fn display_bytecode(bytes: &[u8]) {
    println!("Bytecode:");
    let (ip, constants) = display_constant_pool(bytes);
    println!();
    display_byte_instructions(&constants, &bytes[ip..])
}

fn display_byte_instructions(constants: &[String], bytes: &[u8]) {
    println!("\tInstructions");
    let mut ip = 0;
    let bytes_len = bytes.len();

    let byte_padding = bytes_len.to_string().len();

    while ip < bytes_len {
        let opcode: Opcode = bytes[ip].try_into().expect("not an opcode");
        macro_rules! get_usize {
            () => {
                usize::from_be_bytes((&bytes[ip..ip + 8]).try_into().unwrap())
            };
        }

        print!(
            "#{ip:<padding$}: {:7} ",
            get_opcode_mnemonic(&opcode),
            padding = byte_padding
        );

        ip += 1;
        match opcode {
            Opcode::PushByte => {
                print!("<value {}>", bytes[ip]);
                ip += 1;
            }
            Opcode::PushInt => {
                print!("<value {}>", get_usize!());
                ip += 8;
            }
            Opcode::PushFloat => {
                print!(
                    "<value {}>",
                    f64::from_be_bytes((&bytes[ip..ip + 8]).try_into().unwrap())
                );
                ip += 8;
            }
            Opcode::PushString => {
                let constant_pos = get_usize!();
                let padding = (byte_padding - constant_pos.to_string().len()) + 10;
                print!(
                    "<constant #{}> {:padding$}// \"{}\"",
                    constant_pos,
                    "",
                    constants[constant_pos],
                    padding = padding
                );
                ip += 8;
            }
            Opcode::GetLocal => {
                print!("<local @{}>", bytes[ip]);
                ip += 1;
            }
            Opcode::SetLocal => {
                print!("<constant @{}>", bytes[ip]);
                ip += 1;
            }
            Opcode::Spawn => {
                print!("<arity {}>", bytes[ip]);
                ip += 1;
            }
            Opcode::IfJump => {
                print!("<instruction #{}>", get_usize!());
                ip += 8;
            }
            Opcode::IfNotJump => {
                print!("<instruction #{}>", get_usize!());
                ip += 8;
            }
            Opcode::Jump => {
                print!("<instruction #{}>", get_usize!());
                ip += 8;
            }
            Opcode::PopByte => {}
            Opcode::PopQWord => {}
            Opcode::ConvertIntToStr => {}
            Opcode::ConvertFloatToStr => {}
            Opcode::ConvertIntToByte => {}
            Opcode::Concat => {}
            Opcode::BXor => {}
            Opcode::IntAdd => {}
            Opcode::IntSub => {}
            Opcode::IntMul => {}
            Opcode::IntDiv => {}
            Opcode::IntMod => {}
        }

        println!()
    }
}

fn get_opcode_mnemonic(opcode: &Opcode) -> &'static str {
    match opcode {
        Opcode::PushInt => "pshi",
        Opcode::PushByte => "pshb",
        Opcode::PushFloat => "pshf",
        Opcode::PushString => "pshs",
        Opcode::GetLocal => "get",
        Opcode::SetLocal => "set",
        Opcode::Spawn => "spawn",
        Opcode::PopByte => "pop",
        Opcode::PopQWord => "pop8",
        Opcode::IfJump => "ifjmp",
        Opcode::IfNotJump => "ifnjmp",
        Opcode::Jump => "jmp",
        Opcode::ConvertIntToStr => "i2s",
        Opcode::ConvertFloatToStr => "f2s",
        Opcode::ConvertIntToByte => "i2b",
        Opcode::Concat => "cnct",
        Opcode::BXor => "bxor",
        Opcode::IntAdd => "iadd",
        Opcode::IntSub => "isub",
        Opcode::IntMul => "imul",
        Opcode::IntDiv => "idiv",
        Opcode::IntMod => "imod",
    }
}

fn display_constant_pool(bytes: &[u8]) -> (usize, Vec<String>) {
    println!("\tConstant pool");

    let count = bytes[0];
    let mut current_byte = 1;

    let mut constants = Vec::new();

    let padding = count.to_string().len();
    for constant_id in 0..count {
        let str_len =
            usize::from_be_bytes((&bytes[current_byte..current_byte + 8]).try_into().unwrap());
        current_byte += 8;
        let str = String::from_utf8(bytes[current_byte..current_byte + str_len].to_vec())
            .expect("not utf8");
        current_byte += str_len;

        println!("#{constant_id:<padding$} <string utf-8> // \"{str}\"");
        constants.push(str);
    }

    (current_byte, constants)
}
