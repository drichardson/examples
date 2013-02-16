set( DisplayPicture_LIBS opencv_highgui opencv_core )
set( DisplayPicture_SRCS main.cpp )

include_directories(/opt/local/include)
link_directories (/opt/local/lib) 

add_executable(DisplayPicture ${DisplayPicture_SRCS})
target_link_libraries(DisplayPicture ${DisplayPicture_LIBS})