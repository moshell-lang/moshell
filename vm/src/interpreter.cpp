#include "interpreter.h"
#include "memory/call_stack.h"
#include "conversions.h"
#include "memory/operand_stack.h"

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <sys/wait.h>
#include <unistd.h>

enum Opcode {
    OP_PUSH_INT,    // with 8 byte int value, pushes an int onto the operand stack
    OP_PUSH_BYTE,   // with 1 byte value, pushes a byte onto the operand stack
    OP_PUSH_FLOAT,  // with 8 byte float value, pushes a float onto the operand stack
    OP_PUSH_STRING, // with 8 byte string index in constant pool, pushes a string ref onto the operand stack
    OP_GET_LOCAL,   // with 1 byte local index, pushes given local value onto the operand stack
    OP_SET_LOCAL,   // with 1 byte local index, set given local value from value popped from the operand stack
    OP_SPAWN,       // with 1 byte stack size for process exec(), pushes process exit status onto the operand stack
    OP_INVOKE,      // with 4 byte function signature ref in constant pool, pops parameters from operands then pushes invoked function return in operand stack (if non-void)

    OP_POP_BYTE,   // pops one byte from operand stack
    OP_POP_Q_WORD, // pops 8 instructions from operand stack

    OP_IF_JUMP,     // with 1 byte opcode for 'then' branch, jumps only if value popped from operand stack is 0
    OP_IF_NOT_JUMP, // with 1 byte opcode for where to jump, jumps only if value popped from operand stack is not 0
    OP_JUMP,        // with 1 byte opcode for where to jump

    OP_INT_TO_STR,   // replaces last value of operand stack from int to a string reference
    OP_FLOAT_TO_STR, // replaces last value of operand stack from float to a string reference
    OP_INT_TO_BYTE,  // replaces last value of operand stack from int to byte
    OP_CONCAT,       // pops two string references, concatenates them, and pushes the result


    OP_B_XOR,   // pops last two bytes, apply xor operation then push the result
    OP_INT_ADD, // takes two ints, adds them, and pushes the result
    OP_INT_SUB, // takes two ints, subtracts them, and pushes the result
    OP_INT_MUL, // takes two ints, multiplies them, and pushes the result
    OP_INT_DIV, // takes two ints, divides them, and pushes the result
    OP_INT_MOD, // takes two ints, mods them, and pushes the result
};

void spawn_process(OperandStack &operands, const ConstantPool &pool, int frame_size) {
    // Create argv of the given frame_size, and create a new string for each arg with a null byte after each string
    char **argv = new char *[frame_size + 1];
    for (int i = frame_size - 1; i >= 0; i--) {
        // pop the string index
        int64_t index = operands.pop_string_constant_ref();
        const std::string &arg = pool.get_string(index);
        size_t arg_length = arg.length();
        // Allocate the string
        argv[i] = new char[arg_length + 1];
        // Copy the string data
        memcpy(argv[i], arg.data(), arg_length);
        // Add the null byte
        argv[i][arg_length] = '\0';
    }
    argv[frame_size] = nullptr;

    // Fork and exec the process
    pid_t pid = fork();
    if (pid == 0) {
        // Replace the current process with a new process image
        execvp(argv[0], argv);
        //it technically never returns buts this is to keep the source code semantic
        return;
    }

    for (int i = 0; i < frame_size; i++) {
        delete[] argv[i];
    }
    delete[] argv;
    int status = 0;
    // Wait for the process to finish
    waitpid(pid, &status, 0);

    // Add the exit status to the stack
    // TODO: introduce Exitcode type to push a byte here instead
    operands.push_int(WEXITSTATUS(status));

}

// Apply an arithmetic operation to two integers
inline int64_t apply_op(Opcode code, int64_t a, int64_t b) {
    switch (code) {
    case OP_INT_ADD:
        return a + b;
    case OP_INT_SUB:
        return a - b;
    case OP_INT_MUL:
        return a * b;
    case OP_INT_DIV:
        return a / b;
    case OP_INT_MOD:
        return a % b;
    default:
        std::cerr << "Error: Unknown opcode " << (int)code << "\n";
        exit(1);
    }
}

void push_function_invocation(constant_index callee_signature_idx,
                              const runtime_state &state,
                              OperandStack &caller_operands,
                              CallStack &call_stack) {

    const function_signature &callee_signature = state.pool.get_signature(callee_signature_idx);
    const function_definition &callee_def = state.functions.at(callee_signature_idx);

    // check parameters
    for (Type param_type : callee_signature.params) {
        switch (param_type) {
        case Type::STRING: {
            int64_t str_ref = caller_operands.pop_string_constant_ref();
            if (str_ref < 0 || str_ref >= (int64_t)state.strings.size()) {
                throw MemoryError("Wrong String Reference: popped string from caller operands does not refers to any string");
            }
            break;
        }
        case Type::FLOAT: {
            caller_operands.pop_double();
            break;
        }
        case Type::INT: {
            caller_operands.pop_int();
            break;
        }
        case Type::BYTE: {
            caller_operands.pop_byte();
            break;
        }
        case Type::VOID: {
            // This case should never be activated as the VM checked descriptions before, thus no
            // void parameters would be present at runtime
            throw VirtualMachineError("got void parameter in callee signature");
        }
        }
    }

    call_stack.push_frame(callee_def, callee_signature_idx, caller_operands);
}

/// returns true if this frame has returned
bool run_frame(runtime_state state, stack_frame &frame, CallStack &call_stack, const char *instructions, size_t instruction_count) {
    const ConstantPool &pool = state.pool;

    // the instruction pointer
    size_t &ip = *frame.instruction_pointer;
    OperandStack &operands = frame.operands;
    Locals &locals = frame.locals;

    while (ip < instruction_count) {
        // Read the opcode
        char opcode = instructions[ip++];
        switch (opcode) {
        case OP_PUSH_INT: {
            // Read the 8 byte int value
            int64_t value = ntohl(*(int64_t *)(instructions + ip));
            ip += 8;
            // Push the value onto the stack
            operands.push_int(value);
            break;
        }
        case OP_PUSH_BYTE: {
            char value = *(instructions + ip);
            ip++;
            operands.push_byte(value);
            break;
        }
        case OP_PUSH_FLOAT: {
            // Read the 8 byte float value
            int64_t value = ntohl(*(int64_t *)(instructions + ip));
            ip++;
            // Push the value onto the stack
            operands.push_double(reinterpret_cast<double &>(value));
            break;
        }
        case OP_PUSH_STRING: {
            // Read the string reference
            int64_t index = ntohl(*(int64_t *)(instructions + ip));
            ip += sizeof(int64_t);
            // Push the string index onto the stack
            operands.push_string_constant_ref(index);
            break;
        }
        case OP_SPAWN: {
            // Read the 1 byte stack size
            char frame_size = instructions[ip];
            spawn_process(operands, pool, frame_size);
            break;
        }
        case OP_INVOKE: {
            constant_index signature_idx = ntohl(*(constant_index *)(instructions + ip));
            ip += sizeof(constant_index);

            push_function_invocation(signature_idx, state, operands, call_stack);
            return false; // terminate this frame run
        }
        case OP_GET_LOCAL: {
            // Read the 1 byte local local_index
            char local_index = *(instructions + ip);
            ip++;
            int64_t value = locals.get_int64(local_index);
            // Push the local onto the stack
            operands.push_int(value);
            break;
        }
        case OP_SET_LOCAL: {
            // Read the 1 byte local index
            char index = *(instructions + ip);
            ip++;
            // Pop the value from the stack
            int64_t value = operands.pop_int();
            // Set the local
            locals.set_int64(value, index);
            break;
        }
        case OP_INT_TO_STR: {
            int64_t value = operands.pop_int();

            int64_t string_ref = state.strings.size();
            state.strings.push_back(std::to_string(value));
            operands.push_string_constant_ref(string_ref);

            break;
        }
        case OP_FLOAT_TO_STR: {
            double value = operands.pop_double();

            int64_t string_ref = state.strings.size();
            state.strings.push_back(std::to_string(value));
            operands.push_string_constant_ref(string_ref);

            break;
        }
        case OP_INT_TO_BYTE: {
            int64_t i = operands.pop_int();
            operands.push_byte((char)i);
            break;
        }
        case OP_CONCAT: {
            const std::string& right = pool.get_string(operands.pop_string_constant_ref());
            const std::string& left = pool.get_string(operands.pop_string_constant_ref());

            std::string result = left + right;
            size_t string_ref = state.strings.size();
            state.strings.push_back(result);

            operands.push_string_constant_ref(string_ref);
            break;
        }
        case OP_IF_NOT_JUMP:
        case OP_IF_JUMP: {
            char value = operands.pop_byte();
            u_int32_t then_branch = ntohl(*(u_int32_t *)(instructions + ip + sizeof(char)));
            // test below means "test is true if value is 1 and we are in a if-jump,
            //                    or if value is not 1 and we are in a if-not-jump operation"
            if (value == (instructions[ip] == OP_IF_JUMP)) {
                ip = then_branch;
            } else {
                // the length of branch destination
                ip += sizeof(u_int32_t);
            }
            break;
        }
        case OP_JUMP: {
            size_t destination = ntohl(*(size_t *)(instructions + ip + sizeof(char)));
            ip = destination;
            break;
        }
        case OP_POP_BYTE: {
            operands.pop_byte();
            break;
        }
        case OP_POP_Q_WORD: {
            operands.pop_bytes(8);
            break;
        }
        case OP_B_XOR: {
            char a = operands.pop_byte();
            char b = operands.pop_byte();
            operands.push_byte((char)(a ^ b));
            break;
        }
        case OP_INT_ADD:
        case OP_INT_SUB:
        case OP_INT_MUL:
        case OP_INT_DIV:
        case OP_INT_MOD: {
            int64_t b = operands.pop_int();
            int64_t a = operands.pop_int();
            int64_t res = apply_op(static_cast<Opcode>(instructions[ip]), a, b);
            operands.push_int(res);
            break;
        }
        default: {
            std::cerr << "Error: Unknown opcode " << (int)instructions[ip] << "\n";
            exit(1);
        }
        }
    }
    return true; // this frame has returned
}

void handle_frame_return(Type return_type,
                         OperandStack &caller_operands,
                         OperandStack &frame_operands,
                         const std::vector<std::string> &strings) {
    switch (return_type) {
    case Type::STRING: {
        int64_t str_ref = frame_operands.pop_string_constant_ref();
        if (str_ref < 0 || str_ref >= (int64_t)strings.size()) {
            throw MemoryError("Wrong String Reference: popped string from caller operands does not refers to any string");
        }
        caller_operands.push_string_constant_ref(str_ref);
        break;
    }
    case Type::FLOAT: {
        caller_operands.push_double(frame_operands.pop_double());
        break;
    }
    case Type::INT: {
        caller_operands.push_int(frame_operands.pop_int());
        break;
    }
    case Type::BYTE: {
        caller_operands.push_byte(frame_operands.pop_byte());
        break;
    }
    case Type::VOID: {
    } // do nothing when function returns void
    }
}

int run(runtime_state state, constant_index root_def_idx) {
    const function_definition &root_def = state.functions.at(root_def_idx);
    CallStack call_stack = CallStack::create(1024, root_def, root_def_idx);

    while (!call_stack.is_empty()) {
        stack_frame current_frame = call_stack.peek_frame();
        const function_definition &current_def = state.functions.at(current_frame.function_signature_idx);

        bool has_returned = run_frame(state, current_frame, call_stack, current_def.instructions, current_def.instruction_count);

        if (has_returned) {
            function_signature fs = state.pool.get_signature(current_frame.function_signature_idx);
            call_stack.pop_frame();

            if (call_stack.is_empty()) {
                // the main method returned
                break;
            }

            stack_frame caller_frame = call_stack.peek_frame();

            handle_frame_return(fs.return_type, caller_frame.operands, current_frame.operands, state.strings);
        }
    }

    return 0;
}

int run_module(const module_definition &module_def, std::vector<std::string> &strings) {

    const ConstantPool &pool = module_def.pool;

    // find module main function
    for (auto function : module_def.functions) {
        constant_index signature_id = function.first;
        const function_signature &signature = pool.get_signature(signature_id);

        // we found our main function
        if (signature.name == "<module_main>" && signature.params.empty()) {
            runtime_state state{strings, module_def.functions, pool};

            return run(state, signature_id);
        }
    }

    throw InvalidModuleDescription("Module does not contains any main function");
}
