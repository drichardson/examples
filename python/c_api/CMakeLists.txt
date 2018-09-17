cmake_minimum_required(VERSION 3.7.2)
project(embedded_python)

set(CMAKE_CXX_FLAGS "-std=c++17")

# For You Complete Me code completion
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(PythonLibs)
include_directories(${PYTHON_INCLUDE_DIRS})

add_executable(run-py-main run-py-main.cc)
target_link_libraries(run-py-main ${PYTHON_LIBRARIES})

add_executable(embedded-python embedded-python.cc)
target_link_libraries(embedded-python ${PYTHON_LIBRARIES})

