cmake_minimum_required(VERSION 3.15)
project(opencv)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Find the OpenCV package
find_package(OpenCV REQUIRED)

# Specify the include directories
include_directories(${PROJECT_SOURCE_DIR}/include
                    /usr/lib64/erlang/erts-14.2.4/include
                    /usr/lib64/erlang/usr/include
                    ${OpenCV_INCLUDE_DIRS})

# Specify the source file
set(CALC_BACK_PROJECT ${PROJECT_SOURCE_DIR}/calc_back_project.cpp)

# Create the shared library
add_library(calc_back_project SHARED ${CALC_BACK_PROJECT})

# Link OpenCV libraries
target_link_libraries(calc_back_project ${OpenCV_LIBS})
