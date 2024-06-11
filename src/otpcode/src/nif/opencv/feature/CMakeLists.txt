cmake_minimum_required(VERSION 3.15)
project(opencv)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Set OpenCV directory before finding package
set(OpenCV_DIR /home/linuxbrew/.linuxbrew/Cellar/opencv/4.9.0_9/share/OpenCV)

find_package(OpenCV REQUIRED)

# Specify the include directories
include_directories(${PROJECT_SOURCE_DIR}/include /usr/lib64/erlang/erts-14.2.4/include /usr/lib64/erlang/usr/include)

include_directories(${OpenCV_INCLUDE_DIRS})

# Specify the source file
set(CORNER_EIGEN ${PROJECT_SOURCE_DIR}/corner_eigen.cpp)
set(CORNER_HARRIS ${PROJECT_SOURCE_DIR}/corner_harris.cpp)

# Create the shared library
add_library(corner_eigen SHARED ${CORNER_EIGEN})
add_library(corner_harris SHARED ${CORNER_HARRIS})

# Link OpenCV libraries
target_link_libraries(corner_eigen ${OpenCV_LIBS})
target_link_libraries(corner_harris ${OpenCV_LIBS})

