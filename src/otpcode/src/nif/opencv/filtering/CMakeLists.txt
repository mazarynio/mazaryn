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
    ${PROJECT_SOURCE_DIR}/bilateral.cpp
    ${PROJECT_SOURCE_DIR}/blur.cpp
    ${PROJECT_SOURCE_DIR}/box_filter.cpp
    ${PROJECT_SOURCE_DIR}/build_pyramid.cpp
    ${PROJECT_SOURCE_DIR}/dilate.cpp
)

# Create the shared libraries
add_library(bilateral SHARED ${PROJECT_SOURCE_DIR}/bilateral.cpp)
add_library(blur SHARED ${PROJECT_SOURCE_DIR}/blur.cpp)
add_library(box_filter SHARED ${PROJECT_SOURCE_DIR}/box_filter.cpp)
add_library(build_pyramid SHARED ${PROJECT_SOURCE_DIR}/build_pyramid.cpp)
add_library(dilate SHARED ${PROJECT_SOURCE_DIR}/dilate.cpp)

# Link OpenCV libraries
target_link_libraries(bilateral ${OpenCV_LIBS})
target_link_libraries(blur ${OpenCV_LIBS})
target_link_libraries(box_filter ${OpenCV_LIBS})
target_link_libraries(build_pyramid ${OpenCV_LIBS})
target_link_libraries(dilate ${OpenCV_LIBS})
