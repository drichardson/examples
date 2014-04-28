cmake_minimum_required(VERSION 2.8)
project(lispy)


#
# Build lispy
#
set(CMAKE_CXX_FLAGS "-std=c++11")
add_executable(lispy parser.cpp)


#
# Unit Tests
#
enable_testing()

macro (test_ok file)
    add_test(ok_${file} ${CMAKE_BINARY_DIR}/lispy "${CMAKE_SOURCE_DIR}/test/succeed/${file}.lisp")
endmacro (test_ok)

macro (test_fail file)
    add_test(fail_${file} ${CMAKE_BINARY_DIR}/lispy "${CMAKE_SOURCE_DIR}/test/fail/${file}.lisp")
    set_tests_properties(fail_${file} PROPERTIES
        WILL_FAIL true)
endmacro (test_fail)

test_ok(1)
test_ok(2)
test_ok(3)
test_ok(4)

test_fail(1)
test_fail(2)

#
# Add check target that outputs test results on failure.
#
if (CMAKE_CONFIGURATION_TYPES)
    add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} 
        --force-new-ctest-process --output-on-failure 
        --build-config "$<CONFIGURATION>")
else()
    add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} 
        --force-new-ctest-process --output-on-failure)
endif()

add_dependencies(check lispy)
