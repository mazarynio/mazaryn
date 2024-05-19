cmake_minimum_required(VERSION 3.15) 
project(quantum)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

find_package(qpp REQUIRED)

# Specify the include directories
include_directories(${PROJECT_SOURCE_DIR}/include /usr/lib64/erlang/erts-14.2.4/include /usr/lib64/erlang/usr/include)

# Specify the source file
set(SRC_FILE ${PROJECT_SOURCE_DIR}/chat_dense_coding.cpp)

# Create the shared library
add_library(chat_dense_coding SHARED ${SRC_FILE})

# Set the output directory for the shared library
set_target_properties(chat_dense_coding PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR})

target_link_libraries(chat_dense_coding PUBLIC ${QPP_LINK_DEPS} libqpp)