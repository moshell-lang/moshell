cmake_minimum_required(VERSION 3.15)

project(vm)

if (MSVC)
    add_compile_options(/W4)
else ()
    add_compile_options(-Wall -Wextra -Wpedantic)
endif ()

include_directories(src)

add_library(vm
        src/definitions/bytecode_unit.cpp
        src/memory/call_stack.cpp
        src/memory/constant_pool.cpp
        src/memory/locals.cpp
        src/memory/nix.cpp
        src/memory/operand_stack.cpp
        src/memory/strings.cpp
        src/byte_reader.cpp
        src/errors.cpp
        src/interpreter.cpp
        src/stdlib_natives.cpp
        src/vm.cpp
)
target_compile_features(vm PUBLIC cxx_std_17)
add_executable(vm_exe src/main.cpp)
target_compile_features(vm_exe PUBLIC cxx_std_17)

target_link_libraries(vm_exe vm)

install(TARGETS vm DESTINATION .)
