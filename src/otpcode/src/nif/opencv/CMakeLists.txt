cmake_minimum_required(VERSION 3.10)
project(OpenCV)

set(CMAKE_CXX_STANDARD 11)

# Set OpenCV directory
set(OpenCV_DIR /home/linuxbrew/.linuxbrew/Cellar/opencv/4.9.0_9/include/opencv4/opencv2)

find_package(OpenCV REQUIRED)

include_directories(${OpenCV_INCLUDE_DIRS})

add_executable(OpenCV main.cpp)

target_link_libraries(OpenCV ${OpenCV_LIBS})
