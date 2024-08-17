cmake_minimum_required(VERSION 3.15) 
project(post)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Specify the include directories
include_directories(${PROJECT_SOURCE_DIR}/include /usr/lib64/erlang/erts-14.2.4/include /usr/lib64/erlang/usr/include)

# Specify the source file
set(DEVICE ${PROJECT_SOURCE_DIR}/device.cpp)

# Create the shared library
add_library(device SHARED ${DEVICE})


# Set the output directory for the shared library
set_target_properties(device
                      PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR})

target_link_libraries(device PUBLIC ${QPP_LINK_DEPS}) 
