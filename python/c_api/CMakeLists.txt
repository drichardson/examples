cmake_minimum_required(VERSION 3.7.2)
project(embedded_python)

set(CMAKE_CXX_FLAGS "-std=c++17")

find_package(PythonLibs)
include_directories(${PYTHON_INCLUDE_DIRS})

add_executable(embedded_python main.cc)
target_link_libraries(embedded_python ${PYTHON_LIBRARIES})

