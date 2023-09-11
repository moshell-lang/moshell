cmake_minimum_required(VERSION 3.15)

project(vm)

if (MSVC)
    add_compile_options(/W4)
else ()
    add_compile_options(-Wall -Wextra -Wpedantic)
    add_compile_options($<$<CONFIG:Debug>:-fsanitize=address>)
endif ()

include_directories(src)

add_library(vm
        src/definitions/loader.cpp
        src/definitions/pager.cpp
        src/memory/call_stack.cpp
        src/memory/constant_pool.cpp
        src/memory/locals.cpp
        src/memory/nix.cpp
        src/memory/heap.cpp
        src/memory/operand_stack.cpp
        src/byte_reader.cpp
        src/errors.cpp
        src/interpreter.cpp
        src/stdlib_natives.cpp
        src/vm.cpp
        src/memory/gc.cpp
)
target_compile_features(vm PUBLIC cxx_std_20)
add_executable(vm_exe src/main.cpp)
target_compile_features(vm_exe PUBLIC cxx_std_17)

target_link_options(vm_exe PRIVATE $<$<CONFIG:Debug>:-fsanitize=address>)

target_link_libraries(vm_exe vm)

install(TARGETS vm DESTINATION .)
