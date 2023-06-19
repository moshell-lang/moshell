#include "interpreter.h"
#include "memory/call_stack.h"
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

    OP_POP_BYTE,   // pops one byte from operand stack
    OP_POP_Q_WORD, // pops 8 bytes from operand stack

    OP_IF_JUMP,     // with 1 byte opcode for 'then' branch, jumps only if value popped from operand stack is 0
    OP_IF_NOT_JUMP, // with 1 byte opcode for where to jump, jumps only if value popped from operand stack is not 0
    OP_JUMP,        // with 1 byte opcode for where to jump

    OP_INT_TO_STR,   // replaces last value of operand stack from int to a string reference
    OP_FLOAT_TO_STR, // replaces last value of operand stack from float to a string reference
    OP_INT_TO_BYTE,  // replaces last value of operand stack from int to byte

    OP_B_XOR // pops last two bytes, apply xor operation then push the result
};

void run_instructions(const ConstantPool &pool,
                      const char *bytes,
                      size_t size,
                      OperandStack operands,
                      Locals locals,
                      std::vector<std::string> &strings) {

    unsigned int ip = 0;

    while (ip < size) {
        // Read the opcode
        switch (bytes[ip]) {
        case OP_PUSH_INT: {
            // Read the 8 byte int value
            int64_t value = *(int64_t *)(bytes + ip + 1);
            ip += 9;
            // Push the value onto the stack
            operands.push_int(ntohl(value));
            break;
        }
        case OP_PUSH_BYTE: {
            char value = *(bytes + ip + 1);
            ip += 2;
            operands.push_byte(value);
            break;
        }
        case OP_PUSH_FLOAT: {
            // Read the 8 byte float value
            int64_t value = ntohl(*(int64_t *)(bytes + ip + 1));
            ip += 9;
            // Push the value onto the stack
            operands.push_double(reinterpret_cast<double &>(value));
            break;
        }
        case OP_PUSH_STRING: {
            // Read the string reference
            int64_t index = ntohl(*(int64_t *)(bytes + ip + 1));
            ip += 1 + sizeof(int64_t);
            // Push the string index onto the stack
            operands.push_string_constant_ref(index);
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
            } else {
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
            break;
        }
        case OP_GET_LOCAL: {
            // Read the 1 byte local local_index
            char local_index = *(bytes + ip + 1);
            ip += 2;
            int64_t value = locals.get_int64(local_index);
            // Push the local onto the stack
            operands.push_int(value);
            break;
        }
        case OP_SET_LOCAL: {
            // Read the 1 byte local index
            char index = *(bytes + ip + 1);
            ip += 2;
            // Pop the value from the stack
            int64_t value = operands.pop_int();
            // Set the local
            locals.set_int64(value, index);
            break;
        }
        case OP_INT_TO_STR: {
            int64_t value = operands.pop_int();

            int64_t string_ref = strings.size();
            strings.push_back(std::to_string(value));
            operands.push_string_constant_ref(string_ref);

            // goto next operation
            ip++;
            break;
        }
        case OP_FLOAT_TO_STR: {
            // double value = operands.pop_double();

            int64_t string_ref = 0; // append_str_value(value, pool);
            operands.push_string_constant_ref(string_ref);

            // goto next operation
            ip++;
            break;
        }
        case OP_INT_TO_BYTE: {
            int64_t i = operands.pop_int();
            operands.push_byte((char)i);
            ip++;
            break;
        }
        case OP_IF_NOT_JUMP:
        case OP_IF_JUMP: {
            char value = operands.pop_byte();
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
            operands.pop_byte();
            ip++;
            break;
        }
        case OP_POP_Q_WORD: {
            operands.pop_bytes(8);
            ip++;
            break;
        }
        case OP_B_XOR: {
            char a = operands.pop_byte();
            char b = operands.pop_byte();
            operands.push_byte((char)(a ^ b));
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

void run(const ConstantPool &pool, const char *bytes, size_t size, std::vector<std::string> &strings) {
    auto [call_stack, root_operands, root_locals] =
        CallStack::create(1024, 50, 50);

    run_instructions(pool, bytes, size, root_operands, root_locals, strings);
}
