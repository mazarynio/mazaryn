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

# Specify the source files
set(SOURCES
    ${PROJECT_SOURCE_DIR}/corner_eigen.cpp
    ${PROJECT_SOURCE_DIR}/corner_harris.cpp
)

# Create the shared libraries
add_library(corner_eigen SHARED ${PROJECT_SOURCE_DIR}/corner_eigen.cpp)
add_library(corner_harris SHARED ${PROJECT_SOURCE_DIR}/corner_harris.cpp)

# Link OpenCV libraries
target_link_libraries(corner_eigen ${OpenCV_LIBS})
target_link_libraries(corner_harris ${OpenCV_LIBS})
