#include "interpreter.h"
#include "conversions.h"

#include "memory/call_stack.h"
#include "memory/constant_pool.h"
#include "memory/operand_stack.h"
#include "vm.h"

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <sys/wait.h>
#include <unistd.h>
#include <vector>

enum Opcode {
    OP_PUSH_INT,    // with 8 byte int value, pushes an int onto the operand stack
    OP_PUSH_BYTE,   // with 1 byte value, pushes a byte onto the operand stack
    OP_PUSH_FLOAT,  // with 8 byte float value, pushes a float onto the operand stack
    OP_PUSH_STRING, // with 8 byte string index in constant pool, pushes a string ref onto the operand stack

    OP_GET_BYTE,   // with 1 byte local index, pushes given local value onto the operand stack
    OP_SET_BYTE,   // with 1 byte local index, set_bytes given local value from value popped from the operand stack
    OP_GET_Q_WORD, // with 1 byte local index, pushes given local value onto the operand stack
    OP_SET_Q_WORD, // with 1 byte local index, set_bytes given local value from value popped from the operand stack
    OP_GET_REF,    // with 1 byte local index, pushes given local value onto the operand stack
    OP_SET_REF,    // with 1 byte local index, set_bytes given local value from value popped from the operand stack

    OP_SPAWN,  // with 1 byte stack size for process exec(), pushes process exit status onto the operand stack
    OP_INVOKE, // with 4 byte function ref string in constant pool, pops parameters from operands then pushes invoked function return in operand stack (if non-void)

    OP_POP_BYTE,   // pops one byte from operand stack
    OP_POP_Q_WORD, // pops 8 bytes from operand stack
    OP_POP_REF,    // pops a reference from operand stack, the number of bytes is architecture specific

    OP_IF_JUMP,     // with 1 byte opcode for 'then' branch, jumps only if value popped from operand stack is 0
    OP_IF_NOT_JUMP, // with 1 byte opcode for where to jump, jumps only if value popped from operand stack is not 0
    OP_JUMP,        // with 1 byte opcode for where to jump

    OP_RETURN, // stops frame interpretation

    OP_BYTE_TO_INT,  // replaces last value of operand stack from byte to int
    OP_INT_TO_STR,   // replaces last value of operand stack from int to a string reference
    OP_FLOAT_TO_STR, // replaces last value of operand stack from float to a string reference
    OP_INT_TO_BYTE,  // replaces last value of operand stack from int to byte
    OP_CONCAT,       // pops two string references, concatenates them, and pushes the result

    OP_BYTE_XOR,  // pops last two bytes, apply xor operation then push the resulting byte
    OP_INT_ADD,   // pops two ints, adds them, and pushes the resulting integer
    OP_INT_SUB,   // pops two ints, subtracts them, and pushes the resulting integer
    OP_INT_MUL,   // pops two ints, multiplies them, and pushes the resulting integer
    OP_INT_DIV,   // pops two ints, divides them, and pushes the resulting integer
    OP_INT_MOD,   // pops two ints, mods them, and pushes the resulting integer
    OP_FLOAT_ADD, // pops two floats, adds them, and pushes the resulting float
    OP_FLOAT_SUB, // pops two floats, subtracts them, and pushes the resulting float
    OP_FLOAT_MUL, // pops two floats, multiplies them, and pushes the resulting float
    OP_FLOAT_DIV, // pops two floats, divides them, and pushes the resulting float

    OP_STR_EQ, // pops two string references, checks if they are equal, and pushes the resulting byte
    OP_INT_EQ, // pops two ints, checks if they are equal, and pushes the resulting byte
    OP_INT_LT, // pops two ints, checks if the first is less than the second, and pushes the resulting byte
    OP_INT_LE, // pops two ints, checks if the first is less than or equal to the second, and pushes the resulting byte
    OP_INT_GT, // pops two ints, checks if the first is greater than the second, and pushes the resulting byte
    OP_INT_GE, // pops two ints, checks if the first is greater than or equal to the second, and pushes the resulting byte

    OP_FLOAT_EQ, // pops two floats, checks if they are equal, and pushes the resulting byte
    OP_FLOAT_LT, // pops two floats, checks if the first is less than the second, and pushes the resulting byte
    OP_FLOAT_LE, // pops two floats, checks if the first is less than or equal to the second, and pushes the resulting byte
    OP_FLOAT_GT, // pops two floats, checks if the first is greater than the second, and pushes the resulting byte
    OP_FLOAT_GE, // pops two floats, checks if the first is greater than or equal to the second, and pushes the resulting byte
};

/// contains values needed during runtime interpretation
struct runtime_state {
    /// strings heap space
    strings_t &strings;

    /// loaded function definitions, bound with their string identifier
    const std::unordered_map<const std::string *, function_definition> &functions;

    /// The used constant pool
    const ConstantPool &pool;
};

/**
 * Spawn a new process inside a fork, and wait for its exit status.
 * Will pop from `operand` stack `frame_size` string references elements.
 * where the last popped operand element is the process path.
 * The resulting exitcode status of the child is pushed in the operands stack
 *
 * @param operands the operands to pop arguments / push result
 * @param frame_size number of arguments to pop from the operands, including
 */
void spawn_process(OperandStack &operands, uint8_t frame_size) {
    // Create argv of the given frame_size, and create a new string for each arg with a null byte after each string
    std::vector<std::unique_ptr<char[]>> argv(frame_size + 1);
    for (int i = frame_size - 1; i >= 0; i--) {
        // pop the string index
        size_t reference = operands.pop_reference();
        // cast the ref to a string pointer
        const std::string &arg = *(std::string *)reference;
        size_t arg_length = arg.length() + 1; // add 1 for the trailing '\0' char
        // Allocate the string
        argv[i] = std::make_unique<char[]>(arg_length);
        // copy the string fata
        memcpy(argv[i].get(), arg.c_str(), arg_length);
    }
    argv[frame_size] = nullptr;

    // Fork and exec the process
    pid_t pid = fork();
    if (pid == 0) {
        // Replace the current process with a new process image
        if (execvp(argv[0].get(), reinterpret_cast<char *const *>(argv.data())) == -1) {
            perror("execvp");
            _exit(MOSHELL_COMMAND_NOT_RUNNABLE);
        }
    } else if (pid == -1) {
        perror("fork");
    } else {
        int status = 0;
        // Wait for the process to finish
        waitpid(pid, &status, 0);

        // Add the exit status to the stack
        operands.push_byte(WEXITSTATUS(status) & 0xFF);
    }
}

/**
 * Apply an arithmetic operation to two integers
 * @param code The opcode to apply
 * @param a The first integer
 * @param b The second integer
 * @return The result of the arithmetic operation
 */
inline int64_t apply_arithmetic(Opcode code, int64_t a, int64_t b) {
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
        throw InvalidBytecodeError("Unknown opcode");
    }
}

/**
 * Apply an arithmetic operation to two floats
 * @param code The opcode to apply
 * @param a The first float
 * @param b The second float
 * @return The result of the arithmetic operation
 */
inline double apply_arithmetic(Opcode code, double a, double b) {
    switch (code) {
    case OP_FLOAT_ADD:
        return a + b;
    case OP_FLOAT_SUB:
        return a - b;
    case OP_FLOAT_MUL:
        return a * b;
    case OP_FLOAT_DIV:
        return a / b;
    default:
        throw InvalidBytecodeError("Unknown opcode");
    }
}

/**
 * Apply a comparison operation to two integers
 * @param code The opcode to apply
 * @param a The first integer
 * @param b The second integer
 * @return The result of the comparison
 */
inline bool apply_comparison(Opcode code, int64_t a, int64_t b) {
    switch (code) {
    case OP_INT_EQ:
        return a == b;
    case OP_INT_GT:
        return a > b;
    case OP_INT_GE:
        return a >= b;
    case OP_INT_LT:
        return a < b;
    case OP_INT_LE:
        return a <= b;
    default:
        throw InvalidBytecodeError("Unknown opcode");
    }
}

/**
 * Apply a comparison operation to two floats
 * @param code The opcode to apply
 * @param a The first float
 * @param b The second float
 * @return The result of the comparison
 */
inline bool apply_comparison(Opcode code, double a, double b) {
    switch (code) {
    case OP_FLOAT_EQ:
        return a == b;
    case OP_FLOAT_GT:
        return a > b;
    case OP_FLOAT_GE:
        return a >= b;
    case OP_FLOAT_LT:
        return a < b;
    case OP_FLOAT_LE:
        return a <= b;
    default:
        throw InvalidBytecodeError("Unknown opcode");
    }
}

inline void push_function_invocation(constant_index callee_identifier_idx,
                                     const runtime_state &state,
                                     OperandStack &caller_operands,
                                     CallStack &call_stack) {

    const std::string *callee_identifier = &state.pool.get_string(callee_identifier_idx);
    const function_definition &callee_def = state.functions.at(callee_identifier);

    caller_operands.pop_bytes(callee_def.parameters_byte_count);

    call_stack.push_frame(callee_def, callee_identifier);
}

/**
 * Will run a frame until it returns or pushes a new method inside the call_stack
 * @return true if this function returned because the current frame has ended, or false if it returned because it pushed a new frame
 */
bool run_frame(runtime_state &state, stack_frame &frame, CallStack &call_stack, const char *instructions, size_t instruction_count) {
    const ConstantPool &pool = state.pool;

    // the instruction pointer
    size_t &ip = *frame.instruction_pointer;
    OperandStack &operands = frame.operands;
    Locals &locals = frame.locals;

    while (ip < instruction_count) {
        // Read the opcode
        Opcode opcode = static_cast<Opcode>(instructions[ip++]);
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
            ip += 8;
            // Push the value onto the stack
            operands.push_double(reinterpret_cast<double &>(value));
            break;
        }
        case OP_PUSH_STRING: {
            // Read the string reference
            constant_index index = ntohl(*(constant_index *)(instructions + ip));
            ip += sizeof(constant_index);

            const std::string *str_ref = &pool.get_string(index);

            // Push the string index onto the stack
            operands.push_reference((uint64_t)str_ref);
            break;
        }
        case OP_SPAWN: {
            // Read the 1 byte stack size
            char frame_size = instructions[ip];
            ip++;
            spawn_process(operands, frame_size);
            break;
        }
        case OP_INVOKE: {
            constant_index identifier_idx = ntohl(*(constant_index *)(instructions + ip));
            ip += sizeof(constant_index);

            push_function_invocation(identifier_idx, state, operands, call_stack);
            return false; // terminate this frame run
        }
        case OP_GET_BYTE: {
            // Read the 1 byte local local_index
            uint32_t local_index = ntohl(*(uint32_t *)(instructions + ip));
            ip += 4;
            char value = locals.get_byte(local_index);
            // Push the local onto the stack
            operands.push_byte(value);
            break;
        }
        case OP_SET_BYTE: {
            // Read the 1 byte local index
            uint32_t index = ntohl(*(uint32_t *)(instructions + ip));
            ip += 4;
            // Pop the value from the stack
            char value = operands.pop_byte();
            // Set the local
            locals.set_byte(value, index);
            break;
        }
        case OP_GET_Q_WORD: {
            // Read the 1 byte local local_index
            uint32_t local_index = ntohl(*(uint32_t *)(instructions + ip));
            ip += 4;
            int64_t value = locals.get_q_word(local_index);
            // Push the local onto the stack
            operands.push_int(value);
            break;
        }
        case OP_SET_Q_WORD: {
            // Read the 1 byte local index
            uint32_t index = ntohl(*(uint32_t *)(instructions + ip));
            ip += 4;
            // Pop the value from the stack
            int64_t value = operands.pop_int();
            // Set the local
            locals.set_q_word(value, index);
            break;
        }
        case OP_GET_REF: {
            // Read the 1 byte local local_index
            uint32_t local_index = ntohl(*(uint32_t *)(instructions + ip));
            ip += 4;
            int64_t value = locals.get_ref(local_index);
            // Push the local onto the stack
            operands.push_reference(value);
            break;
        }
        case OP_SET_REF: {
            // Read the 1 byte local index
            uint32_t index = ntohl(*(uint32_t *)(instructions + ip));
            ip += 4;
            // Pop the value from the stack
            uint64_t value = operands.pop_reference();
            // Set the local
            locals.set_ref(value, index);
            break;
        }
        case OP_BYTE_TO_INT: {
            char value = operands.pop_byte();
            operands.push_int(value);
            break;
        }
        case OP_INT_TO_STR: {
            int64_t value = operands.pop_int();

            auto [it, _] = state.strings.insert(std::make_unique<std::string>(std::to_string(value)));
            operands.push_reference((uint64_t)it->get());

            break;
        }
        case OP_FLOAT_TO_STR: {
            double value = operands.pop_double();

            auto [it, _] = state.strings.insert(std::make_unique<std::string>(std::to_string(value)));
            operands.push_reference((uint64_t)it->get());

            break;
        }
        case OP_INT_TO_BYTE: {
            int64_t i = operands.pop_int();
            operands.push_byte((char)i);
            break;
        }
        case OP_CONCAT: {
            auto right = (const std::string *)operands.pop_reference();
            auto left = (const std::string *)operands.pop_reference();

            std::string result = *left + *right;

            auto [it, _] = state.strings.insert(std::make_unique<std::string>(result));
            operands.push_reference((uint64_t)it->get());
            break;
        }
        case OP_IF_NOT_JUMP:
        case OP_IF_JUMP: {
            char value = operands.pop_byte();
            uint32_t then_branch = ntohl(*(uint32_t *)(instructions + ip));
            // test below means "test is true if value is 1 and we are in a if-jump,
            //                    or if value is not 1 and we are in a if-not-jump operation"
            if (value == (opcode == OP_IF_JUMP)) {
                ip = then_branch;
            } else {
                // the length of branch destination
                ip += sizeof(uint32_t);
            }
            break;
        }
        case OP_JUMP: {
            uint32_t destination = ntohl(*(uint32_t *)(instructions + ip));
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
        case OP_POP_REF: {
            operands.pop_reference();
            break;
        }
        case OP_BYTE_XOR: {
            char a = operands.pop_byte();
            char b = operands.pop_byte();
            operands.push_byte(a ^ b);
            break;
        }
        case OP_INT_ADD:
        case OP_INT_SUB:
        case OP_INT_MUL:
        case OP_INT_DIV:
        case OP_INT_MOD: {
            int64_t b = operands.pop_int();
            int64_t a = operands.pop_int();
            int64_t res = apply_arithmetic(opcode, a, b);
            operands.push_int(res);
            break;
        }
        case OP_FLOAT_ADD:
        case OP_FLOAT_SUB:
        case OP_FLOAT_MUL:
        case OP_FLOAT_DIV: {
            double b = operands.pop_double();
            double a = operands.pop_double();
            double res = apply_arithmetic(opcode, a, b);
            operands.push_double(res);
            break;
        }
        case OP_STR_EQ: {
            const std::string &b = *(const std::string *)operands.pop_reference();
            const std::string &a = *(const std::string *)operands.pop_reference();
            operands.push_byte(a == b);
            break;
        }
        case OP_INT_EQ:
        case OP_INT_LT:
        case OP_INT_LE:
        case OP_INT_GT:
        case OP_INT_GE: {
            int64_t b = operands.pop_int();
            int64_t a = operands.pop_int();
            char res = apply_comparison(opcode, a, b);
            operands.push_byte(res);
            break;
        }
        case OP_FLOAT_EQ:
        case OP_FLOAT_LT:
        case OP_FLOAT_LE:
        case OP_FLOAT_GT:
        case OP_FLOAT_GE: {
            double b = operands.pop_double();
            double a = operands.pop_double();
            char res = apply_comparison(opcode, a, b);
            operands.push_byte(res);
            return true;
        }
        case OP_RETURN:
            return true;

        default: {
            throw InvalidBytecodeError("Unknown opcode " + std::to_string(opcode));
        }
        }
    }
    return true; // this frame has returned
}

/**
 * runs the interpreter, where the first function to be executed
 * is the given identifier
 */
void run(runtime_state state, const std::string *root_identifier) {
    // prepare the call stack, containing the given root function on top of the stack
    const function_definition &root_def = state.functions.at(root_identifier);
    CallStack call_stack = CallStack::create(10000, root_def, root_identifier);

    while (!call_stack.is_empty()) {
        stack_frame current_frame = call_stack.peek_frame();
        const function_definition &current_def = state.functions.at(current_frame.function_identifier);

        bool has_returned = run_frame(state, current_frame, call_stack, current_def.instructions, current_def.instruction_count);

        if (has_returned) {
            int8_t returned_byte_count = current_def.return_byte_count;
            const char *bytes = current_frame.operands.pop_bytes(returned_byte_count);
            // place returned bytes in the first index of locals
            current_frame.locals.set_bytes(bytes, returned_byte_count, 0);

            // pop the current frame.
            call_stack.pop_frame();

            if (call_stack.is_empty()) {
                // the root method has returned
                break;
            }
            stack_frame caller_frame = call_stack.peek_frame();

            // JUSTIFY: the returned value is placed at the very first index of locals.
            // locals of the callee frame and operands of the caller frame are adjacent in memory thanks to
            // call stack's layout. This operation will transfer the return value in the operand stack of the caller
            // by advancing the caller's operands of the declared byte count in the callee's function definition.
            //
            // JUSTIFY: This also cannot cause UB as a minimal frame size is equal to `sizeof(frame_headers)`,
            // that contains ints and pointers, which is greater than the maximum return byte count allowed of `sizeof(uint64_t)`.
            // stack overflow is impossible here because the callee frame would already have caused it earlier.
            caller_frame.operands.advance_unchecked(returned_byte_count);
        }
    }
}

void run_unit(const bytecode_unit &module_def, strings_t &strings) {

    const ConstantPool &pool = module_def.pool;

    // find module main function
    for (auto function : module_def.functions) {
        const std::string &identifier = *function.first;

        // we found our main function, we search for a function named `<main>` with no parameters, regardless of the return type
        if (identifier.rfind("::<main>\0", identifier.length() - strlen("<main>")) != identifier.npos) {
            runtime_state state{strings, module_def.functions, pool};

            run(std::move(state), &identifier);
            return;
        }
    }

    throw InvalidBytecodeStructure("Module does not contains any `<main>()` function");
}
