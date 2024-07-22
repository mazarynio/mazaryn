cmake_minimum_required(VERSION 3.15)
project(p2p)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Find Boost
find_package(Boost REQUIRED COMPONENTS system)
# Find OpenSSL
find_package(OpenSSL REQUIRED)
# Find libcurl
find_package(CURL REQUIRED)
# Find the libnice library
find_package(PkgConfig REQUIRED)
pkg_check_modules(LIBNICE REQUIRED nice)
pkg_check_modules(GLIB REQUIRED glib-2.0)

# Specify the include directories
include_directories(${PROJECT_SOURCE_DIR}/include /usr/lib64/erlang/erts-14.2.4/include /usr/lib64/erlang/usr/include)

# Include Boost headers
include_directories(${Boost_INCLUDE_DIRS}) 
include_directories(${OPENSSL_INCLUDE_DIR})
include_directories(${CURL_INCLUDE_DIRS})

# Specify the source file
set(UTILS_NIF ${PROJECT_SOURCE_DIR}/utils_nif.cpp)
set(FILE_NIF ${PROJECT_SOURCE_DIR}/file_nif.cpp)
set(UUID_NIF ${PROJECT_SOURCE_DIR}/uuid_nif.cpp)
set(HASH_NIF ${PROJECT_SOURCE_DIR}/hash_nif.cpp)

# Create the shared library
add_library(utils_nif SHARED ${UTILS_NIF})
add_library(file_nif SHARED ${FILE_NIF})
add_library(uuid_nif SHARED ${UUID_NIF})
add_library(hash_nif SHARED ${HASH_NIF})

target_link_libraries(utils_nif ${Boost_LIBS} OpenSSL::SSL OpenSSL::Crypto ${CURL_LIBRARIES})
target_link_libraries(file_nif ${Boost_LIBS} OpenSSL::SSL OpenSSL::Crypto ${CURL_LIBRARIES})
target_link_libraries(uuid_nif ${Boost_LIBS} OpenSSL::SSL OpenSSL::Crypto ${CURL_LIBRARIES})
target_link_libraries(hash_nif ${Boost_LIBS} OpenSSL::SSL OpenSSL::Crypto ${CURL_LIBRARIES})
