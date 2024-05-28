cmake_minimum_required(VERSION 3.15) 
project(quantum)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

find_package(qpp REQUIRED)

# Specify the include directories
include_directories(${PROJECT_SOURCE_DIR}/include /usr/lib64/erlang/erts-14.2.4/include /usr/lib64/erlang/usr/include)

# Specify the source file
set(CHAT_DENSE_CODING ${PROJECT_SOURCE_DIR}/chat_dense_coding.cpp)
set(QPE ${PROJECT_SOURCE_DIR}/qpe.cpp)
set(ENTANGLEMENT ${PROJECT_SOURCE_DIR}/entanglement.cpp)
set(RANDOMNESS ${PROJECT_SOURCE_DIR}/randomness.cpp)
set(GROVER ${PROJECT_SOURCE_DIR}/grover.cpp)
set(TELEPORT_QUDIT ${PROJECT_SOURCE_DIR}/teleport_qudit.cpp)
set(TELEPORT_QUBIT ${PROJECT_SOURCE_DIR}/teleport_qubit.cpp)

# Create the shared library
add_library(chat_dense_coding SHARED ${CHAT_DENSE_CODING})
add_library(qpe SHARED ${QPE})
add_library(entanglement SHARED ${ENTANGLEMENT})
add_library(randomness SHARED ${RANDOMNESS})
add_library(grover SHARED ${GROVER})
add_library(teleport_qudit SHARED ${TELEPORT_QUDIT})
add_library(teleport_qubit SHARED ${TELEPORT_QUBIT})

# Set the output directory for the shared library
set_target_properties(chat_dense_coding qpe entanglement randomness 
                      PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR})

target_link_libraries(chat_dense_coding PUBLIC ${QPP_LINK_DEPS} libqpp)
target_link_libraries(qpe PUBLIC ${QPP_LINK_DEPS} libqpp)
target_link_libraries(entanglement PUBLIC ${QPP_LINK_DEPS} libqpp)
target_link_libraries(randomness PUBLIC ${QPP_LINK_DEPS} libqpp)
target_link_libraries(grover PUBLIC ${QPP_LINK_DEPS} libqpp)
target_link_libraries(teleport_qudit PUBLIC ${QPP_LINK_DEPS} libqpp)
target_link_libraries(teleport_qubit PUBLIC ${QPP_LINK_DEPS} libqpp)