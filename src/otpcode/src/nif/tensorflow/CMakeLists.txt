cmake_minimum_required(VERSION 3.15)
project(tensorflow)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Set the TensorFlow root directory
set(TENSORFLOW_ROOT /tensorflow)

# Include TensorFlow headers
include_directories(${TENSORFLOW_ROOT})

# Link TensorFlow library
link_directories(${TENSORFLOW_ROOT}/bazel-bin/tensorflow)

# Add the executable
add_executable(tf_cpp_project main.cpp)

# Link the TensorFlow libraries
target_link_libraries(tf_cpp_project tensorflow_cc tensorflow_framework)
