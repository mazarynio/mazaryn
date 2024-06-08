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
set(BILATERAL ${PROJECT_SOURCE_DIR}/bilateral.cpp)
set(BLUR ${PROJECT_SOURCE_DIR}/blur.cpp)
set(BOX_FILTER ${PROJECT_SOURCE_DIR}/box_filter.cpp)
set(BUILD_PYRAMID ${PROJECT_SOURCE_DIR}/build_pyramid.cpp)
set(DILATE ${PROJECT_SOURCE_DIR}/dilate.cpp)

# Create the shared library
add_library(bilateral SHARED ${BILATERAL})
add_library(blur SHARED ${BLUR})
add_library(box_filter SHARED ${BOX_FILTER})
add_library(build_pyramid SHARED ${BUILD_PYRAMID})
add_library(dilate SHARED ${DILATE})

# Link OpenCV libraries
target_link_libraries(bilateral ${OpenCV_LIBS})
target_link_libraries(blur ${OpenCV_LIBS})
target_link_libraries(box_filter ${OpenCV_LIBS})
target_link_libraries(build_pyramid ${OpenCV_LIBS})
target_link_libraries(dilate ${OpenCV_LIBS})
