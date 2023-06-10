cmake_minimum_required(VERSION 3.10)

project(vm)

set(CMAKE_CXX_STANDARD 17)

if (CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic -g")
endif()
if (MSVC)
    set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} /W4 /WX")
endif()

add_library(vm src/main.cpp src/interpreter.cpp src/vm.cpp)
add_executable(vm_exe src/main.cpp src/interpreter.cpp src/vm.cpp)

install(TARGETS vm DESTINATION .)
