cmake_minimum_required(VERSION 3.10)

project(vm)

set(CMAKE_CXX_STANDARD 17)

if (CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic -g")
endif()
if (MSVC)
    set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} /W4 /WX")
endif()

add_library(vm src/interpreter.cpp src/vm.cpp src/conversions.cpp src/memory/operand_stack.cpp src/pool.cpp)
add_executable(vm_exe src/main.cpp)
target_link_libraries(vm_exe vm)

install(TARGETS vm DESTINATION .)
