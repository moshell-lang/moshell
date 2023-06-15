#include "interpreter.h"
#include "memory/operand_stack.h"
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <sys/wait.h>
#include <unistd.h>

enum Opcode {
    OP_PUSH_INT,    // with 8 byte int value, pushes an int onto the operand stack
    OP_PUSH_FLOAT,  // with 8 byte float value, pushes a float onto the operand stack
    OP_PUSH_STRING, // with 8 byte string index in constant pool, pushes a string ref onto the operand stack
    OP_GET_LOCAL,   // with 1 byte local index, pushes given local value onto the operand stack
    OP_SET_LOCAL,   // with 1 byte local index, set given local value from value popped from the operand stack
    OP_SPAWN,       // with 1 byte stack size for process exec(), pushes process exit status onto the operand stack

    OP_POP_BYTE,    // pops one byte from operand stack
    OP_POP_Q_WORD,  // pops 8 bytes from operand stack

    OP_IF_JUMP,     // with 8 byte opcode for 'then' branch, jumps only if value popped from operand stack is 0
    OP_IF_NOT_JUMP, // with 8 byte opcode for where to jump, jumps only if value popped from operand stack is not 0
    OP_JUMP,        // with 8 byte opcode for where to jump

    OP_INT_TO_STR,  // replaces last value of operand stack from int to a string reference
    OP_FLOAT_TO_STR,// replaces last value of operand stack from float to a string reference
};


constant_pool::constant_pool(int capacity) {
    strings.reserve(capacity);
    sizes.reserve(capacity);
}


void handle_process_state(int status, char current_instruction, unsigned int& ip, OperandStack& stack) {
    // look ahead for OP_POP_BYTE instruction in order to avoid useless push/pop operations
    // which often happens
    if (current_instruction == OP_POP_BYTE) {
        ip++; //skip pop operation and do not push
        return;
    }
    // add status to the stack
    stack.push_int(status);
}


void run(constant_pool pool, const char* bytes, size_t size) {
    std::unique_ptr<char[]> locals_buf = std::make_unique<char[]>(1024);
    OperandStack stack = OperandStack(1024);

    unsigned int ip = 0;
    char* locals = locals_buf.get();
    int local_frame = 0;
    while (ip < size) {
        // Read the opcode
        switch (bytes[ip]) {
            case OP_PUSH_INT: {
                // Read the 8 byte int value
                int64_t value = *(int64_t*) (bytes + ip + 1);
                ip += 9;
                // Push the value onto the stack
                stack.push_int(ntohl(value));
                break;
            }
            case OP_PUSH_FLOAT: {
                // Read the 8 byte float value
                int64_t value = ntohl(*(int64_t*) (bytes + ip + 1));
                ip += 9;
                // Push the value onto the stack
                stack.push_double(reinterpret_cast<double&>(value));
                break;
            }
            case OP_PUSH_STRING: {
                // Read the string reference
                int64_t index = ntohl(*(int64_t*) (bytes + ip + 1));
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
                char** argv = new char* [frame_size + 1];
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
                    // Execute the process then exit
                    exit(execvp(argv[0], argv));
                } else {
                    for (int i = 0; i < frame_size; i++) {
                        delete[] argv[i];
                    }
                    delete[] argv;
                    int status;
                    // Wait for the process to finish
                    waitpid(pid, &status, 0);

                    handle_process_state(status, bytes[ip], ip, stack);
                }
                break;
            }
            case OP_GET_LOCAL: {
                // Read the 1 byte local local_index
                char local_index = *(bytes + ip + 1);
                ip += 2;
                // Push the local onto the stack
                stack.push_int(*(int64_t*) (locals + local_frame + local_index * 8));
                break;
            }
            case OP_SET_LOCAL: {
                // Read the 1 byte local index
                char index = *(bytes + ip + 1);
                ip += 2;
                // Pop the value from the stack
                int64_t value = stack.pop_int();
                // Set the local
                *(int64_t*) (locals + local_frame + index * 8) = value;
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
            case OP_IF_NOT_JUMP:
            case OP_IF_JUMP : {
                int64_t value = stack.pop_int();
                size_t then_branch = ntohl(*(size_t*) (bytes + ip + sizeof(char)));
                // as we are in a shell interpreter thus
                // when value is 0 it means that the test or operation succeeded.
                // the test is reversed if current operation is OP_IF_NOT_JUMP
                if ((value == 0) == (bytes[ip] == OP_IF_JUMP)) {
                    ip = then_branch;
                } else {
                    //the length of if-jump opcode and its branch destination
                    ip += 1 + sizeof(size_t);
                }
                break;
            }
            case OP_JUMP: {
                size_t destination = ntohl(*(size_t*) (bytes + ip + sizeof(char)));
                ip = destination;
                break;
            }
            case OP_POP_BYTE: {
                stack.pop_bytes(1);
                ip++;
                break;
            }
            case OP_POP_Q_WORD: {
                stack.pop_bytes(8);
                ip++;
                break;
            }
            default: {
                std::cerr << "Error: Unknown opcode " << (int) bytes[ip] << "\n";
                exit(1);
            }
        }
    }
}

