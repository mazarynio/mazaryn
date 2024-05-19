cmake_minimum_required(VERSION 3.10) 
project(print_string2)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Specify the include directories
include_directories(${PROJECT_SOURCE_DIR}/include /usr/lib64/erlang/erts-14.2.4/include /usr/lib64/erlang/usr/include)

# Specify the source file
set(SRC_FILE ${PROJECT_SOURCE_DIR}/print_string2.cpp)

# Create the shared library
add_library(print_string2 SHARED ${SRC_FILE})

# Set the output directory for the shared library
set_target_properties(print_string2 PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR})