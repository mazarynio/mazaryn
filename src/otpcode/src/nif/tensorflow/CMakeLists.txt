cmake_minimum_required(VERSION 3.10)
project(TensorFlowCppProject)

# Set the path to TensorFlow include and library directories
set(TENSORFLOW_INCLUDE_DIR /usr/local/include/tensorflow)
set(TENSORFLOW_LIBRARY /usr/local/lib/libtensorflow_cc.so)

# Specify the C++ standard
set(CMAKE_CXX_STANDARD 17)

# Add executable target
add_executable(my_executable main.cpp)

# Include TensorFlow headers
include_directories(${TENSORFLOW_INCLUDE_DIR})

# Link TensorFlow library
target_link_libraries(my_executable ${TENSORFLOW_LIBRARY})
