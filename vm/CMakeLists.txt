cmake_minimum_required(VERSION 3.10)

project(vm)

set(CMAKE_CXX_STANDARD 17)

if (CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic -g -O0")
endif ()
if (MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4 /WX")
endif ()

include_directories(src)

add_library(vm src/interpreter.cpp src/vm.cpp src/memory/operand_stack.cpp src/memory/call_stack.cpp src/memory/locals.cpp src/errors.cpp src/memory/constant_pool.cpp src/definitions/function_definition.cpp src/types.cpp src/definitions/module_definition.cpp src/byte_reader.cpp)
add_executable(vm_exe src/main.cpp src/memory/call_stack.cpp src/memory/locals.cpp src/definitions/function_definition.cpp src/byte_reader.cpp)

target_link_libraries(vm_exe vm)

install(TARGETS vm DESTINATION .)
