### Yoinked from https://github.com/JamesBoer/Heady/blob/master/CMakeLists.txt

# Require minimum standard version of CMake
cmake_minimum_required (VERSION 3.17)

function(set_compiler_options project_name)
    target_compile_options(
            ${project_name} PRIVATE
            -Wall -Wextra -pedantic -Werror -Wnon-virtual-dtor
            -Wold-style-cast -Wcast-align -Wunused -Woverloaded-virtual -Wconversion
            -Wsign-conversion -Wmisleading-indentation -Wduplicated-cond
    )
endfunction()

# Set C++ 17 compiler flags
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

# Enable IDE folders
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Set project name
project(ppga-cpp)

# Add source files and dependencies to executable
set(
        heady_source_list
        "ppga.hpp"
        "main.cpp"
)
add_executable(${PROJECT_NAME} ${heady_source_list})
target_link_libraries(${PROJECT_NAME} PRIVATE "stdc++fs")

# Set compiler options
set_compiler_options(${PROJECT_NAME})

option(PACKAGE_TESTS "Build the tests" ON)
if(PACKAGE_TESTS)
    enable_testing()
    include(GoogleTest)
    add_subdirectory(tests)
endif()