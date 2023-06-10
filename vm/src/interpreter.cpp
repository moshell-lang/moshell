#include "interpreter.h"
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <sys/wait.h>
#include <unistd.h>

enum Opcode {
    OP_PUSH_INT,    // 1 byte opcode, 8 byte int value
    OP_PUSH_FLOAT,  // 1 byte opcode, 8 byte float value
    OP_PUSH_STRING, // 1 byte opcode, 1 byte string index in constant pool
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_POP,
    OP_SPAWN, // 1 byte opcode, 1 byte stack size for process exec()
    OP_INT_TO_STR,
    OP_FLOAT_TO_STR,
};



constant_pool::constant_pool(int capacity) {
    strings.reserve(capacity);
    sizes.reserve(capacity);
}




void run(constant_pool pool, int ip, const char *bytes, size_t size) {
    std::unique_ptr<char[]> stack_buf = std::make_unique<char[]>(1024);
    std::unique_ptr<char[]> locals_buf = std::make_unique<char[]>(1024);
    char *stack = stack_buf.get();
    char *locals = locals_buf.get();
    int local_frame = 0;
    int stack_frame = 0;
    while (ip < size) {
        // Read the opcode
        switch (bytes[ip]) {
        case OP_PUSH_INT: {
            // Read the 8 byte int value
            int64_t value = *(int64_t *)(bytes + ip + 1);
            ip += 9;
            // Push the value onto the stack
            *(int64_t *)(stack + stack_frame) = ntohl(value);
            stack_frame += 8;
            break;
        }
        case OP_PUSH_FLOAT: {
            // Read the 8 byte float value
            int64_t value = ntohl(*(int64_t *)(bytes + ip + 1));
            ip += 9;
            // Push the value onto the stack
            *(double *)(stack + stack_frame) = reinterpret_cast<double&>(value);
            stack_frame += 8;
            break;
        }
        case OP_PUSH_STRING: {
            // Read the 1 byte string index
            char index = *(bytes + ip + 1);
            ip += 2;
            // Push the string index onto the stack
            *(int64_t *)(stack + stack_frame) = index;
            stack_frame += 8;
            break;
        }
        case OP_SPAWN: {
            // Read the 1 byte stack size
            int frame_size = *(bytes + ip + 1);
            ip += 2;

            // Create argv of the given frame_size, and create a new string for each arg with a null byte after each string
            char **argv = new char *[frame_size + 1];
            for (int i = 0; i < frame_size; i++) {
                // Read the string index
                int index = *(int64_t *)(stack + stack_frame - (frame_size - i) * 8);
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
                // Execute the process
                execvp(argv[0], argv);
            } else {
                for (int i = 0; i < frame_size; i++) {
                    delete[] argv[i];
                }
                delete[] argv;
                // Wait for the process to finish
                waitpid(pid, nullptr, 0);
                // Pop the stack
                stack_frame -= frame_size;
            }
            break;
        }
        case OP_GET_LOCAL: {
            // Read the 1 byte local local_index
            char local_index = *(bytes + ip + 1);
            ip += 2;
            // Push the local onto the stack
            *(int64_t *)(stack + stack_frame) = *(int64_t *)(locals + local_frame + local_index * 8);
            stack_frame += 8;
            break;
        }
        case OP_SET_LOCAL: {
            // Read the 1 byte local index
            char index = *(bytes + ip + 1);
            ip += 2;
            // Pop the value from the stack
            stack_frame -= 8;
            // Set the local
            *(int64_t *)(locals + local_frame + index * 8) = *(int64_t *)(stack + stack_frame);
            break;
        }
        case OP_POP: {
            // Pop the value from the stack
            stack_frame -= 8;
            ip += 1;
            break;
        }
        case OP_INT_TO_STR: {
            // convert last local from stack to str
            // the local in stack
            auto* stack_local = (int64_t *)(stack + stack_frame - 8);

            int64_t value = *stack_local;
            append_str_value(value, stack_local, pool);

            // goto next operation
            ip++;
            break;
        }
        case OP_FLOAT_TO_STR: {
            // convert last local from stack to str
            // the local in stack
            auto* stack_local = (int64_t *)(stack + stack_frame - 8);

            auto value = *(double *)(stack + stack_frame - 8);
            append_str_value(value, stack_local, pool);
            // goto next operation
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
