cmake_minimum_required(VERSION 3.15)
project(e2ee)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Find OpenSSL
find_package(OpenSSL REQUIRED)

# Specify the include directories
include_directories(${PROJECT_SOURCE_DIR}/include /usr/lib64/erlang/erts-14.2.4/include /usr/lib64/erlang/usr/include)

include_directories(${OPENSSL_INCLUDE_DIR})

# Specify the source file
set(E2EE ${PROJECT_SOURCE_DIR}/e2ee.cpp)


# Create the shared library
add_library(e2ee SHARED ${E2EE})

target_link_libraries(e2ee ${Boost_LIBS} OpenSSL::SSL OpenSSL::Crypto ${CURL_LIBRARIES})
 