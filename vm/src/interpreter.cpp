#include "interpreter.h"
#include "conversions.h"
#include "memory/operand_stack.h"
#include "pool.h"

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

    OP_POP_BYTE,   // pops one byte from operand stack
    OP_POP_Q_WORD, // pops 8 bytes from operand stack

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

// Apply an arithmetic operation to two integers
int64_t apply_op(Opcode code, int64_t a, int64_t b) {
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

void run(constant_pool pool, const char *bytes, size_t size) {
    std::unique_ptr<char[]> locals_buf = std::make_unique<char[]>(1024);
    OperandStack stack = OperandStack(1024);

    unsigned int ip = 0;
    char *locals = locals_buf.get();
    int local_frame = 0;
    while (ip < size) {
        // Read the opcode
        switch (bytes[ip]) {
        case OP_PUSH_INT: {
            // Read the 8 byte int value
            int64_t value = *(int64_t *)(bytes + ip + 1);
            ip += 9;
            // Push the value onto the stack
            stack.push_int(ntohl(value));
            break;
        }
        case OP_PUSH_BYTE: {
            char value = *(bytes + ip + 1);
            ip += 2;
            stack.push_byte(value);
            break;
        }
        case OP_PUSH_FLOAT: {
            // Read the 8 byte float value
            int64_t value = ntohl(*(int64_t *)(bytes + ip + 1));
            ip += 9;
            // Push the value onto the stack
            stack.push_double(reinterpret_cast<double &>(value));
            break;
        }
        case OP_PUSH_STRING: {
            // Read the string reference
            int64_t index = ntohl(*(int64_t *)(bytes + ip + 1));
            ip += 1 + sizeof(int64_t);
            // Push the string index onto the stack
            stack.push_string_constant_ref(index);
            break;
        }
        case OP_SPAWN: {
            // Read the 1 byte stack size
            int frame_size = *(bytes + ip + 1);
            ip += 2;

            // Create argv of the given frame_size, and create a new string for each arg with a null byte after each string
            char **argv = new char *[frame_size + 1];
            for (int i = frame_size - 1; i >= 0; i--) {
                // pop the string index
                int64_t index = stack.pop_string_constant_ref();
                // Allocate the string
                argv[i] = new char[pool.sizes[index] + 1];
                // Copy the string data
                memcpy(argv[i], pool.strings[index].get(), pool.sizes[index]);
                // Add the null byte
                argv[i][pool.sizes[index]] = '\0';
            }
            argv[frame_size] = nullptr;

            // Fork and exec the process
            pid_t pid = fork();
            if (pid == 0) {
                // Replace the current process with a new process image
                execvp(argv[0], argv);
            } else {
                for (int i = 0; i < frame_size; i++) {
                    delete[] argv[i];
                }
                delete[] argv;
                int status = 0;
                // Wait for the process to finish
                waitpid(pid, &status, 0);

                // Add the exit status to the stack
                stack.push_byte(WEXITSTATUS(status) & 0xFF);
            }
            break;
        }
        case OP_GET_LOCAL: {
            // Read the 1 byte local local_index
            char local_index = *(bytes + ip + 1);
            ip += 2;
            // Push the local onto the stack
            stack.push_int(*(int64_t *)(locals + local_frame + local_index * 8));
            break;
        }
        case OP_SET_LOCAL: {
            // Read the 1 byte local index
            char index = *(bytes + ip + 1);
            ip += 2;
            // Pop the value from the stack
            int64_t value = stack.pop_int();
            // Set the local
            *(int64_t *)(locals + local_frame + index * 8) = value;
            break;
        }
        case OP_INT_TO_STR: {
            int64_t value = stack.pop_int();

            int64_t string_ref = append_str_value(value, pool);
            stack.push_string_constant_ref(string_ref);

            // goto next operation
            ip++;
            break;
        }
        case OP_FLOAT_TO_STR: {
            double value = stack.pop_double();

            int64_t string_ref = append_str_value(value, pool);
            stack.push_string_constant_ref(string_ref);

            // goto next operation
            ip++;
            break;
        }
        case OP_INT_TO_BYTE: {
            int64_t i = stack.pop_int();
            stack.push_byte((char)i);
            ip++;
            break;
        }
        case OP_CONCAT: {
            int64_t right = stack.pop_string_constant_ref();
            int64_t left = stack.pop_string_constant_ref();

            int64_t string_ref = pool.concat(left, right);
            stack.push_string_constant_ref(string_ref);

            ip++;
            break;
        }
        case OP_IF_NOT_JUMP:
        case OP_IF_JUMP: {
            char value = stack.pop_byte();
            size_t then_branch = ntohl(*(size_t *)(bytes + ip + sizeof(char)));
            // test below means "test is true if value is 1 and we are in a if-jump,
            //                    or if value is not 1 and we are in a if-not-jump operation"
            if (value == (bytes[ip] == OP_IF_JUMP)) {
                ip = then_branch;
            } else {
                // the length of if-jump opcode and its branch destination
                ip += 1 + sizeof(size_t);
            }
            break;
        }
        case OP_JUMP: {
            size_t destination = ntohl(*(size_t *)(bytes + ip + sizeof(char)));
            ip = destination;
            break;
        }
        case OP_POP_BYTE: {
            stack.pop_byte();
            ip++;
            break;
        }
        case OP_POP_Q_WORD: {
            stack.pop_bytes(8);
            ip++;
            break;
        }
        case OP_B_XOR: {
            char a = stack.pop_byte();
            char b = stack.pop_byte();
            stack.push_byte((char)(a ^ b));
            ip++;
            break;
        }
        case OP_INT_ADD:
        case OP_INT_SUB:
        case OP_INT_MUL:
        case OP_INT_DIV:
        case OP_INT_MOD: {
            int64_t b = stack.pop_int();
            int64_t a = stack.pop_int();
            int64_t res = apply_op(static_cast<Opcode>(bytes[ip]), a, b);
            stack.push_int(res);
            ip++;
            break;
        }
        default: {
            std::cerr << "Error: Unknown opcode " << (int)bytes[ip] << "\n";
            exit(1);
        }
        }
    }
}
