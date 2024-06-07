cmake_minimum_required(VERSION 3.10)
project(TensorFlowCpp)

# Set the TensorFlow directory
set(TENSORFLOW_SOURCE_DIR /tensorflow)

# Include TensorFlow headers
include_directories(${TENSORFLOW_SOURCE_DIR})

# Find TensorFlow libraries
link_directories(${TENSORFLOW_SOURCE_DIR}/bazel-bin/tensorflow)

# Add executable
add_executable(your_application main.cpp)

# Link TensorFlow libraries
target_link_libraries(your_application tensorflow_cc)
