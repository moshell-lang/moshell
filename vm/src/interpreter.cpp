#include "interpreter.h"
#include "conversions.h"

#include "definitions/loader.h"
#include "definitions/pager.h"
#include "memory/call_stack.h"
#include "memory/constant_pool.h"
#include "memory/gc.h"
#include "memory/nix.h"
#include "vm.h"

#include <array>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <memory>
#include <sys/wait.h>
#include <unistd.h>
#include <vector>

enum Opcode {
    OP_PUSH_INT,        // with 8 byte int value, pushes an int onto the operand stack
    OP_PUSH_BYTE,       // with 1 byte value, pushes a byte onto the operand stack
    OP_PUSH_FLOAT,      // with 8 byte float value, pushes a float onto the operand stack
    OP_PUSH_STRING_REF, // with 8 byte string index in constant pool, pushes a reference to the string constant onto the operand stack
    OP_PUSH_LOCAL_REF,  // with 4 bytes locals index, pushes a reference to the locals address onto the stack

    OP_BOX_Q_WORD, // pops an int, and push it as a new reference
    OP_BOX_BYTE,   // pops an int, and push it as a new reference
    OP_UNBOX,      // pops a reference, and convert it to a value

    OP_LOCAL_GET_BYTE,   // pops last reference and pushes its byte value onto the operands
    OP_LOCAL_SET_BYTE,   // pops last reference, pops a byte value then sets the reference's value with byte value
    OP_LOCAL_GET_Q_WORD, // pops last reference and pushes its qword value onto the operands
    OP_LOCAL_SET_Q_WORD, // pops last reference, pops a qword value then sets the reference's value with qword value

    OP_REF_GET_BYTE,   // pops last reference and pushes its byte value onto the operands
    OP_REF_SET_BYTE,   // pops last reference, pops a byte value then sets the reference's value with byte value
    OP_REF_GET_Q_WORD, // pops last reference and pushes its qword value onto the operands
    OP_REF_SET_Q_WORD, // pops last reference, pops a qword value then sets the reference's value with qword value

    OP_STRUCT_GET_BYTE,   // pops last reference and pushes byte value at given index onto the operands
    OP_STRUCT_SET_BYTE,   // pops last reference, pops new byte value and set byte value at given index in the struct
    OP_STRUCT_GET_Q_WORD, // pops last reference and pushes qword value at given index onto the operands
    OP_STRUCT_SET_Q_WORD, // pops last reference, pops new qword value and set sword value at given index in the struct

    OP_FETCH_BYTE,   // with 4 byte external index in constant pool, pushes given external value onto the operand stack
    OP_FETCH_Q_WORD, // with 4 byte external index in constant pool, pushes given external value onto the operand stack
    OP_STORE_BYTE,   // with 4 byte external index in constant pool, set given external value from value popped from the operand stack
    OP_STORE_Q_WORD, // with 4 byte external index in constant pool, set given external value from value popped from the operand stack

    OP_STRUCT_NEW,    // with 4 byte external index in constant pool, instantiates on the heap the given structure
    OP_STRUCT_COPY_N, // with 4 byte uint32 pops structure ref, and pop given amount of bytes from the operands to copy them on the given structure, starting at index 0

    OP_INVOKE,         // with 4 byte function ref string in constant pool, pops parameters from operands then pushes invoked function return in operand stack (if non-void)
    OP_FORK,           // forks a new process, pushes the pid onto the operand stack of the parent and jumps to the given address in the parent
    OP_EXEC,           // pops the arguments array and replaces the current program
    OP_WAIT,           // pops a pid from the operand stack and waits for it to finish
    OP_OPEN,           // opens a file with the name popped from the stack, pushes the file descriptor onto the operand stack
    OP_CLOSE,          // pops a file descriptor from the operand stack and closes the file
    OP_SETUP_REDIRECT, // peek the fd from the operand stack, pop the source fd from the operand stack, and performs a cancelable redirection
    OP_REDIRECT,       // duplicates the file descriptor popped from the operand stack and leave the source fd on the stack
    OP_POP_REDIRECT,   // pops a file descriptor from the operand stack and closes it
    OP_PIPE,           // creates a pipe, pushes the read and write file descriptors onto the operand stack
    OP_READ,           // pops a file descriptor to read all the data from, pushes the data onto the stack
    OP_WRITE,          // pops a file descriptor to write the data to, pops the data to write from the stack
    OP_EXIT,           // exits the current process with the popped exit code

    OP_DUP,        // duplicates the last value on the operand stack
    OP_DUP_BYTE,   // duplicates the last byte on the operand stack
    OP_SWAP,       // swaps the last two values on the operand stack
    OP_SWAP_2,     // swaps the last two values on the operand stack with the one before that
    OP_POP_BYTE,   // pops one byte from operand stack
    OP_POP_Q_WORD, // pops 8 bytes from operand stack

    OP_IF_JUMP,     // with 1 byte opcode for 'then' branch, jumps only if value popped from operand stack is 0
    OP_IF_NOT_JUMP, // with 1 byte opcode for where to jump, jumps only if value popped from operand stack is not 0
    OP_JUMP,        // with 1 byte opcode for where to jump

    OP_RETURN, // stops frame interpretation

    OP_BYTE_TO_INT, // replaces last value of operand stack from byte to int
    OP_INT_TO_BYTE, // replaces last value of operand stack from int to byte

    OP_BYTE_XOR,  // pops last two bytes, apply xor operation then push the resulting byte
    OP_INT_ADD,   // pops two ints, adds them, and pushes the resulting integer
    OP_INT_SUB,   // pops two ints, subtracts them, and pushes the resulting integer
    OP_INT_MUL,   // pops two ints, multiplies them, and pushes the resulting integer
    OP_INT_DIV,   // pops two ints, divides them, and pushes the resulting integer
    OP_INT_MOD,   // pops two ints, mods them, and pushes the resulting integer
    OP_INT_NEG,   // pops an int, negates it, and pushes the resulting integer
    OP_FLOAT_ADD, // pops two floats, adds them, and pushes the resulting float
    OP_FLOAT_SUB, // pops two floats, subtracts them, and pushes the resulting float
    OP_FLOAT_MUL, // pops two floats, multiplies them, and pushes the resulting float
    OP_FLOAT_DIV, // pops two floats, divides them, and pushes the resulting float
    OP_FLOAT_NEG, // pops a float, negates it, and pushes the resulting float

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

/**
 * set to true if this process is the main vm's process
 */
bool is_master = true;

/**
 * contains values needed during runtime interpretation
 */
struct runtime_state {
    /**
     * The file descriptor table
     */
    fd_table table;

    /**
     * The loader used to load external symbols
     */
    const msh::loader &loader;

    msh::pager &pager;

    /**
     * native functions pointers, bound with their string identifier
     */
    const natives_functions_t &native_functions;

    /**
     * The process group id, owned by the master shell process.
     *
     * If zero, the process shouldn't be put in a process group.
     */
    pid_t pgid;
};

RuntimeException::RuntimeException(std::string msg)
    : std::runtime_error(msg) {}

// run GC every n objects allocated
#define GC_HEAP_CYCLE 5

runtime_memory::runtime_memory(msh::heap &heap, std::vector<std::string> &program_arguments, msh::gc &gc)
    : heap{heap}, gc{gc}, pargs{program_arguments}, last_gc_heap_size{heap.size()} {}

void runtime_memory::run_gc() {
    gc.run();
    last_gc_heap_size = heap.size();
}

std::vector<std::string> &runtime_memory::program_arguments() {
    return pargs;
}

msh::obj &runtime_memory::emplace(msh::obj_data &&data) {
    if (heap.size() >= last_gc_heap_size + GC_HEAP_CYCLE)
        run_gc();
    return this->heap.insert(data);
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
        if (b == 0) {
            throw RuntimeException("Attempted to divide " + std::to_string(a) + " by zero.");
        }
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
        if (b == 0) {
            throw RuntimeException("Attempted to divide " + std::to_string(a) + " by zero.");
        }
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

void panic(const std::string &msg, CallStack &stack) {
    std::cerr << "panic: " << msg;

    while (!stack.is_empty()) {
        stack_frame &frame = stack.peek_frame();
        const function_definition &def = frame.function;
        std::cerr << "\n\tat " << def.identifier;

        if (!def.mappings.empty()) {
            size_t instruction_line = 0;
            for (const auto &[instruction, line] : def.mappings) {
                if (instruction >= frame.instruction_pointer) {
                    break;
                }
                instruction_line = line;
            }
            std::cerr << " (line " << instruction_line << ")";
        }
        stack.pop_frame();
    }
    std::cerr << std::endl;
    if (!is_master) {
        _exit(MOSHELL_PANIC);
    }
}

/**
 * The outcome of a frame execution.
 */
enum class frame_status {
    /**
     * The frame has been executed successfully.
     */
    RETURNED,

    /**
     * The frame has been interrupted by a new frame.
     */
    NEW_FRAME,

    /**
     * The frame has been interrupted by a panic.
     */
    ABORT
};

/**
 * Handles function invocation.
 * This function performs invocation for either moshell functions (bytecode instructions)
 * and native functions.
 * Moshell functions have priority against native functions.
 *
 * if given function identifier refers to a moshell function, the called function's frame will
 * be pushed in the call stack, which will cause the current frame to interrupt.
 * if a native function is referenced, then the function is directly run by this
 * function and then the frame can simply continue without interruption.
 * @param callee_identifier_idx constant index to the function identifier to invoke
 * @param state the runtime state, passed to native function invocation
 * @param caller_operands caller's operands
 * @param call_stack the call stack
 * @throws FunctionNotFoundError if given callee identifier does not points to a moshell or native function.
 * @return true if a new moshell function has been pushed onto the stack.
 */
inline bool handle_function_invocation(const std::string &callee_identifier,
                                       runtime_state &state,
                                       runtime_memory &mem,
                                       OperandStack &caller_operands,
                                       CallStack &call_stack) {

    auto callee_def_it = state.loader.find_function(callee_identifier);

    if (callee_def_it == state.loader.functions_cend()) {
        auto native_function_it = state.native_functions.find(callee_identifier);
        if (native_function_it == state.native_functions.end()) {
            throw FunctionNotFoundError("Could not find function " + callee_identifier);
        }

        auto native_function = native_function_it->second;
        native_function(caller_operands, mem);

        return false;
    }

    const function_definition &callee_def = callee_def_it->second;

    call_stack.push_frame(callee_def);
    return true;
}

/**
 * Will run a frame until it returns or pushes a new method inside the call_stack
 * @return the frame status
 */
frame_status run_frame(runtime_state &state, stack_frame &frame, CallStack &call_stack, const std::byte *instructions, size_t instruction_count, runtime_memory &mem) {
    size_t pool_index = frame.function.constant_pool_index;
    const ConstantPool &pool = state.pager.get_pool(pool_index);

    // the instruction pointer
    size_t &ip = frame.instruction_pointer;
    OperandStack &operands = frame.operands;
    Locals &locals = frame.locals;

    auto implement_fetch = [&]<typename T>() mutable {
        uint32_t dynsym_index = msh::read_big_endian<uint32_t>(instructions + ip);
        ip += 4;
        T value = state.pager.get<T>(pool_index, dynsym_index);
        operands.push<T>(value);
    };

    auto implement_store = [&]<typename T>() mutable {
        uint32_t dynsym_index = msh::read_big_endian<uint32_t>(instructions + ip);
        ip += 4;
        T value = operands.pop<T>();
        state.pager.set<T>(pool_index, dynsym_index, value);
    };

    while (ip < instruction_count) {
        // Read the opcode
        Opcode opcode = static_cast<Opcode>(instructions[ip++]);
        switch (opcode) {
        case OP_PUSH_INT: {
            // Read the 8 byte int value
            int64_t value = msh::read_big_endian<int64_t>(instructions + ip);
            ip += 8;
            // Push the value onto the stack
            operands.push_int(value);
            break;
        }
        case OP_PUSH_BYTE: {
            std::byte value = *(instructions + ip);
            ip++;
            operands.push_byte(static_cast<int8_t>(value));
            break;
        }
        case OP_PUSH_FLOAT: {
            // Read the 8 byte float value
            double value = msh::read_big_endian<double>(instructions + ip);
            ip += 8;
            // Push the value onto the stack
            operands.push_double(value);
            break;
        }
        case OP_PUSH_STRING_REF: {
            // Read the string reference
            constant_index index = msh::read_big_endian<constant_index>(instructions + ip);
            ip += sizeof(constant_index);

            // Push the string index onto the stack
            msh::obj &ref = const_cast<msh::obj &>(pool.get_ref(index));
            operands.push_reference(ref); // Promise not to modify the string
            break;
        }
        case OP_PUSH_LOCAL_REF: {
            // Read the locals address
            int32_t local_index = msh::read_big_endian<int32_t>(instructions + ip);
            ip += sizeof(int32_t);

            uint8_t *ref = &locals.reference(local_index);

            // Push the local reference onto the stack
            operands.push_unchecked_reference(ref);
            break;
        }
        case OP_STRUCT_GET_BYTE: {
            int32_t struct_index = msh::read_big_endian<int32_t>(instructions + ip);
            ip += sizeof(int32_t);

            msh::obj_struct &structure = operands.pop_reference().get<msh::obj_struct>();
            int8_t byte = structure.bytes[struct_index];
            operands.push_byte(byte);
            break;
        }
        case OP_STRUCT_SET_BYTE: {
            int32_t struct_index = msh::read_big_endian<int32_t>(instructions + ip);
            ip += sizeof(int32_t);

            int8_t byte = operands.pop_byte();
            msh::obj &obj = operands.pop_reference();
            msh::obj_struct &structure = obj.get<msh::obj_struct>();
            structure.bytes[struct_index] = byte;
            break;
        }
        case OP_STRUCT_GET_Q_WORD: {
            int32_t struct_index = msh::read_big_endian<int32_t>(instructions + ip);
            ip += sizeof(int32_t);

            msh::obj &obj = operands.pop_reference();
            msh::obj_struct &structure = obj.get<msh::obj_struct>();
            uint64_t qword = *(uint64_t *)(structure.bytes.data() + struct_index);
            operands.push_int(qword);
            break;
        }
        case OP_STRUCT_SET_Q_WORD: {
            int32_t struct_index = msh::read_big_endian<int32_t>(instructions + ip);
            ip += sizeof(int32_t);

            int64_t qword = operands.pop_int();
            msh::obj &obj = operands.pop_reference();
            msh::obj_struct &structure = obj.get<msh::obj_struct>();
            *(int64_t *)(structure.bytes.data() + struct_index) = qword;
            break;
        }
        case OP_BOX_Q_WORD: {
            // Pop the value
            int64_t value = operands.pop_int();

            // Push the reference onto the stack
            operands.push_reference(mem.emplace(value));
            break;
        }
        case OP_BOX_BYTE: {
            // Pop the value
            int8_t value = operands.pop_byte();

            // Push the reference onto the stack
            operands.push_reference(mem.emplace(value));
            break;
        }
        case OP_UNBOX: {
            // Pop the reference
            msh::obj &ref = operands.pop_reference();

            // Push the value onto the stack
            std::visit([&](auto &&arg) {
                using T = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<T, int64_t>) {
                    operands.push_int(arg);
                } else if constexpr (std::is_same_v<T, double>) {
                    operands.push_double(arg);
                } else if constexpr (std::is_same_v<T, int8_t>) {
                    operands.push_byte(arg);
                } else {
                    throw InvalidBytecodeError("Cannot unbox unknown type");
                }
            },
                       ref.get_data());
            break;
        }
        case OP_STRUCT_NEW: {
            constant_index identifier_idx = msh::read_big_endian<constant_index>(instructions + ip);
            ip += sizeof(constant_index);
            const std::string &identifier = pool.get_string(identifier_idx);

            auto struct_def_it = state.loader.find_structure(identifier);

            if (struct_def_it == state.loader.structures_cend()) {
                throw InvalidBytecodeError("Unknown structure `" + identifier + "`");
                return frame_status::ABORT;
            }

            const msh::struct_definition &struct_def = struct_def_it->second;

            msh::obj &obj = mem.emplace(msh::obj_struct{
                &struct_def,
                std::vector<char>(struct_def.heap_size)});

            operands.push_reference(obj);
            break;
        }
        case OP_STRUCT_COPY_N: {
            uint32_t count = msh::read_big_endian<uint32_t>(instructions + ip);
            ip += sizeof(uint32_t);

            msh::obj &obj = operands.pop_reference();
            msh::obj_struct &structure = obj.get<msh::obj_struct>();
            const std::byte *bytes = operands.pop_bytes(count);
            memcpy(structure.bytes.data(), bytes, count);
            operands.push_reference(obj);
            break;
        }
        case OP_INVOKE: {
            constant_index identifier_idx = msh::read_big_endian<constant_index>(instructions + ip);
            ip += sizeof(constant_index);

            const std::string &function_identifier = pool.get_string(identifier_idx);

            if (handle_function_invocation(function_identifier, state, mem, operands, call_stack)) {
                // terminate this frame interpretation if a new frame has been pushed in the stack
                // (natives functions are directly run thus no need to return if no moshell function is to execute)
                return frame_status::NEW_FRAME;
            }
            break;
        }
        case OP_FORK: {
            uint32_t parent_jump = msh::read_big_endian<uint32_t>(instructions + ip);
            ip += sizeof(uint32_t);
            pid_t pid = fork();
            switch (pid) {
            case -1:
                panic(strerror(errno), call_stack);
                return frame_status::ABORT;
            case 0:
                // Child process
                is_master = false;
                if (state.pgid != 0) {
                    // Put the process into the process group
                    pid = getpid();
                    setpgid(pid, state.pgid);

                    // Set the handling for job control signals back to the default
                    signal(SIGINT, SIG_DFL);
                    signal(SIGQUIT, SIG_DFL);
                    signal(SIGTSTP, SIG_DFL);
                    signal(SIGTTIN, SIG_DFL);
                    signal(SIGTTOU, SIG_DFL);
                }
#ifndef NDEBUG
                msh::disable_gc_debug();
#endif
                break;
            default:
                // Parent process
                ip = parent_jump;
                operands.push_int(static_cast<int>(pid));
                if (state.pgid != 0) {
                    // Add the child process to the process group of the terminal
                    setpgid(pid, state.pgid);
                }
                break;
            }
            break;
        }
        case OP_EXEC: {
            // Read the 1 byte stack size
            const msh::obj_vector &args = operands.pop_reference().get<msh::obj_vector>();

            // Create argv of the given frame_size, and create a new string for each arg with a null byte after each string
            std::vector<const char *> argv(args.size() + 1);
            std::transform(args.begin(), args.end(), argv.begin(), [](const msh::obj *arg) {
                return arg->get<const std::string>().c_str();
            });

            // Replace the current process with a new process image
            if (execvp(argv[0], const_cast<char *const *>(argv.data())) == -1) {
                std::string command = argv[0];
                panic("Unable to execute command \"" + command + "\": " + std::string(strerror(errno)), call_stack);
                return frame_status::ABORT;
            }
            break;
        }
        case OP_WAIT: {
            // Pop the pid
            pid_t pid = static_cast<pid_t>(operands.pop_int());

            int status = 0;
            // Wait for the process to finish
            if (waitpid(pid, &status, 0) == -1) {
                panic(strerror(errno), call_stack);
                return frame_status::ABORT;
            }
            status = WEXITSTATUS(status) & 0xFF;

            // Add the exit status to the stack
            if (status == MOSHELL_PANIC) {
                call_stack.clear();
                return frame_status::ABORT;
            }
            operands.push_byte(status);
            break;
        }
        case OP_OPEN: {
            // Pop the path
            const std::string &path = operands.pop_reference().get<const std::string>();

            // Read the flags
            int flags = static_cast<int>(msh::read_big_endian<int32_t>(instructions + ip));

            // Open the file
            int fd = open(path.c_str(), flags, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
            if (fd == -1) {
                panic("Cannot open file \"" + path + "\": " + std::string(strerror(errno)), call_stack);
                return frame_status::ABORT;
            }

            // Push the file descriptor onto the stack
            operands.push_int(fd);
            ip += sizeof(int);
            break;
        }
        case OP_CLOSE: {
            // Pop the file descriptor
            int fd = static_cast<int>(operands.pop_int());

            // Close the file
            close(fd);
            break;
        }
        case OP_SETUP_REDIRECT: {
            // Pop the file descriptors
            int fd2 = static_cast<int>(operands.pop_int());
            int fd1 = static_cast<int>(operands.pop_int());

            // Redirect the file descriptors
            if (state.table.push_redirection(fd1, fd2) == -1) {
                panic("Unable to redirect " + std::to_string(fd1) + " to " + std::to_string(fd2) + ": " + strerror(errno), call_stack);
                return frame_status::ABORT;
            }
            operands.push_int(fd1);
            break;
        }
        case OP_REDIRECT: {
            // Pop the file descriptors
            int fd2 = operands.pop_int();
            int fd1 = operands.pop_int();

            // Redirect the file descriptors
            if (dup2(fd1, fd2) == -1) {
                panic("Unable to redirect " + std::to_string(fd1) + " to " + std::to_string(fd2) + ": " + strerror(errno), call_stack);
                return frame_status::ABORT;
            }
            operands.push_int(fd1);
            break;
        }
        case OP_POP_REDIRECT: {
            state.table.pop_redirection();
            break;
        }
        case OP_PIPE: {
            // Create the pipe
            int pipefd[2];
            if (pipe(pipefd) == -1) {
                panic("Cannot create pipeline : " + std::string(strerror(errno)), call_stack);
                return frame_status::ABORT;
            }

            // Push the file descriptors onto the stack
            operands.push_int(pipefd[0]);
            operands.push_int(pipefd[1]);
            break;
        }
        case OP_READ: {
            // Pop the file descriptor
            int fd = static_cast<int>(operands.pop_int());

            std::string out;
            std::array<char, 4096> buffer;
            ssize_t r;
            do {
                r = read(fd, buffer.data(), buffer.size());
                if (r == -1) {
                    if (errno != EAGAIN && errno != EINTR) {
                        panic(strerror(errno), call_stack);
                        return frame_status::ABORT;
                    }
                }
                if (r > 0) {
                    out.append(buffer.data(), r);
                }
            } while (r != 0);

            // Remove trailing `\n`
            if (!out.empty() && out.back() == '\n') {
                out.pop_back();
            }

            // Push the string onto the stack
            msh::obj &str = mem.emplace(std::move(out));
            operands.push_reference(str);
            break;
        }
        case OP_WRITE: {
            // Pop the string reference
            const std::string &str = operands.pop_reference().get<const std::string>();
            // Pop the file descriptor
            int fd = static_cast<int>(operands.pop_int());

            // Write the string to the file
            if (write(fd, str.data(), str.length()) == -1) {
                panic("Cannot write in fd " + std::to_string(fd) + ": " + strerror(errno), call_stack);
                return frame_status::ABORT;
            }
            close(fd);
            break;
        }
        case OP_EXIT: {
            // Pop the exit code
            char exit_code = operands.pop_byte();
            exit(static_cast<int>(exit_code));
        }
        case OP_REF_GET_BYTE: {
            char value = (char &)operands.pop_reference();
            operands.push_byte(value);
            break;
        }
        case OP_REF_SET_BYTE: {
            char &value = (char &)operands.pop_reference();
            value = operands.pop_byte();
            break;
        }
        case OP_REF_GET_Q_WORD: {
            int64_t value = (int64_t &)operands.pop_reference();
            operands.push_int(value);
            break;
        }
        case OP_REF_SET_Q_WORD: {
            int64_t &value = (int64_t &)operands.pop_reference();
            value = operands.pop_int();
            break;
        }
        case OP_LOCAL_GET_BYTE: {
            int32_t local_index = msh::read_big_endian<int32_t>(instructions + ip);
            ip += sizeof(int32_t);
            operands.push_byte(locals.get_byte(local_index));
            break;
        }
        case OP_LOCAL_SET_BYTE: {
            int32_t local_index = msh::read_big_endian<int32_t>(instructions + ip);
            ip += sizeof(int32_t);
            locals.set_byte(operands.pop_byte(), local_index);
            break;
        }
        case OP_LOCAL_GET_Q_WORD: {
            int32_t local_index = msh::read_big_endian<int32_t>(instructions + ip);
            ip += sizeof(int32_t);
            int64_t value = locals.get_q_word(local_index);
            operands.push_int(value);
            break;
        }
        case OP_LOCAL_SET_Q_WORD: {
            int32_t local_index = msh::read_big_endian<int32_t>(instructions + ip);
            ip += sizeof(int32_t);
            locals.set_q_word(operands.pop_int(), local_index);
            break;
        }
        case OP_FETCH_BYTE: {
            implement_fetch.template operator()<uint8_t>();
            break;
        }
        case OP_FETCH_Q_WORD: {
            implement_fetch.template operator()<int64_t>();
            break;
        }
        case OP_STORE_BYTE: {
            implement_store.template operator()<uint8_t>();
            break;
        }
        case OP_STORE_Q_WORD: {
            implement_store.template operator()<int64_t>();
            break;
        }
        case OP_BYTE_TO_INT: {
            char value = operands.pop_byte();
            operands.push_int(value);
            break;
        }
        case OP_INT_TO_BYTE: {
            int64_t i = operands.pop_int();
            operands.push_byte(static_cast<int8_t>(i));
            break;
        }
        case OP_IF_NOT_JUMP:
        case OP_IF_JUMP: {
            char value = operands.pop_byte();
            uint32_t then_branch = msh::read_big_endian<uint32_t>(instructions + ip);
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
            uint32_t destination = msh::read_big_endian<uint32_t>(instructions + ip);
            ip = destination;
            break;
        }
        case OP_DUP: {
            operands.dup_qword();
            break;
        }
        case OP_DUP_BYTE: {
            char value = operands.pop_byte();
            operands.push_byte(value);
            operands.push_byte(value);
            break;
        }
        case OP_SWAP: {
            operands.swap_upper_qwords();
            break;
        }
        case OP_SWAP_2: {
            operands.swap_upper_three_qwords();
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
        case OP_INT_NEG: {
            int64_t a = operands.pop_int();
            operands.push_int(-a);
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
        case OP_FLOAT_NEG: {
            double a = operands.pop_double();
            operands.push_double(-a);
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
            break;
        }
        case OP_RETURN:
            return frame_status::RETURNED;

        default: {
#ifdef NDEBUG
#ifdef __GNUC__
            __builtin_unreachable();
#else
            __assume(false);
#endif
#else
            throw InvalidBytecodeError("Unknown opcode " + std::to_string(opcode));
#endif
        }
        }
    }
    return frame_status::RETURNED; // this frame has returned
}

bool run_unit(CallStack &call_stack, const msh::loader &loader, msh::pager &pager, const msh::memory_page &current_page, runtime_memory mem, const natives_functions_t &natives, pid_t pgid) {
    fd_table table;
    runtime_state state{table, loader, pager, natives, pgid};

    // prepare the call stack, containing the given root function on top of the stack
    const function_definition &root_def = loader.get_function(current_page.init_function_name);
    call_stack.push_frame(root_def);

    try {
        while (!call_stack.is_empty()) {
            stack_frame &current_frame = call_stack.peek_frame();
            const function_definition &current_def = current_frame.function;

            const std::byte *instructions = loader.get_instructions(current_def.instructions_start);
            frame_status status = run_frame(state, current_frame, call_stack, instructions, current_def.instruction_count, mem);

            switch (status) {
            case frame_status::RETURNED: {
                int8_t returned_byte_count = current_def.return_byte_count;

                // pop the current frame.
                call_stack.pop_frame();

                if (call_stack.is_empty()) {
                    // the root method has returned
                    break;
                }
                stack_frame &caller_frame = call_stack.peek_frame();
                caller_frame.operands.transfer(current_frame.operands, returned_byte_count);
                break;
            }
            case frame_status::ABORT: {
                return false;
            }
            case frame_status::NEW_FRAME: {
                // continue
            }
            }
        }
        return true;
    } catch (const VirtualMachineError &e) {
        panic("An unexpected Virtual Machine Error occurred.\n" + std::string(e.name()) + " : " + e.what(), call_stack);
    } catch (const RuntimeException &e) {
        panic(e.what(), call_stack);
    } catch (const std::exception &e) {
        panic("An unexpected internal error occurred.\nwhat : " + std::string(e.what()), call_stack);
    }
    return false;
}
