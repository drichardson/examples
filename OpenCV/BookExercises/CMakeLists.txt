set( common_LIBS opencv_highgui opencv_core opencv_imgproc )

include_directories(/usr/local/include)
link_directories (/usr/local/lib) 

add_executable(Exercise3.1 Exercise3.1.c)
target_link_libraries(Exercise3.1 ${common_LIBS})

add_executable(Exercise3.2 Exercise3.2.c)
target_link_libraries(Exercise3.2 ${common_LIBS})

add_executable(Exercise3.3 Exercise3.3.c)
target_link_libraries(Exercise3.3 ${common_LIBS})

add_executable(Exercise3.4 Exercise3.4.c)
target_link_libraries(Exercise3.4 ${common_LIBS})

add_executable(Exercise3.5 Exercise3.5.c)
target_link_libraries(Exercise3.5 ${common_LIBS})

add_executable(Exercise3.6 Exercise3.6.c)
target_link_libraries(Exercise3.6 ${common_LIBS})

add_executable(Exercise3.7 Exercise3.7.c)
target_link_libraries(Exercise3.7 ${common_LIBS})

add_executable(Exercise3.8 Exercise3.8.c)
target_link_libraries(Exercise3.8 ${common_LIBS})

add_executable(Exercise5.1 Exercise5.1.cpp)
target_link_libraries(Exercise5.1 ${common_LIBS})

add_executable(Exercise5.2 Exercise5.2.c)
target_link_libraries(Exercise5.2 ${common_LIBS})

add_executable(Exercise5.3 Exercise5.3.c)
target_link_libraries(Exercise5.3 ${common_LIBS})

add_executable(Exercise5.4 Exercise5.4.c)
target_link_libraries(Exercise5.4 ${common_LIBS})

add_executable(Exercise5.5 Exercise5.5.c)
target_link_libraries(Exercise5.5 ${common_LIBS})

add_executable(Exercise5.6 Exercise5.6.c)
target_link_libraries(Exercise5.6 ${common_LIBS})

add_executable(Exercise5.7 Exercise5.7.c)
target_link_libraries(Exercise5.7 ${common_LIBS})

add_executable(Exercise5.8 Exercise5.8.c)
target_link_libraries(Exercise5.8 ${common_LIBS})

add_executable(Exercise5.9 Exercise5.9.c)
target_link_libraries(Exercise5.9 ${common_LIBS})

add_executable(Exercise5.10 Exercise5.10.c)
target_link_libraries(Exercise5.10 ${common_LIBS})

add_executable(Exercise5.11 Exercise5.11.c)
target_link_libraries(Exercise5.11 ${common_LIBS})

add_executable(Exercise5.12 Exercise5.12.c)
target_link_libraries(Exercise5.12 ${common_LIBS})

add_executable(Exercise6.1 Exercise6.1.c)
target_link_libraries(Exercise6.1 ${common_LIBS})

add_executable(Exercise6.2 Exercise6.2.c)
target_link_libraries(Exercise6.2 ${common_LIBS})

add_executable(Exercise6.4 Exercise6.4.c)
target_link_libraries(Exercise6.4 ${common_LIBS})

add_executable(Exercise6.5 Exercise6.5.c)
target_link_libraries(Exercise6.5 ${common_LIBS})

add_executable(Exercise6.6 Exercise6.6.c)
target_link_libraries(Exercise6.6 ${common_LIBS})
