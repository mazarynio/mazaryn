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
set(GRABCUT ${PROJECT_SOURCE_DIR}/grabcut.cpp)
set(WATERSHED ${PROJECT_SOURCE_DIR}/watershed.cpp)

# Create the shared library
add_library(grabcut SHARED ${GRABCUT})
add_library(watershed SHARED ${WATERSHED})

# Link OpenCV libraries
target_link_libraries(grabcut ${OpenCV_LIBS})
target_link_libraries(watershed ${OpenCV_LIBS})
