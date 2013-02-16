#import <stdio.h>
#import <stdlib.h>
#import <OpenGL/OpenGL.h>

int main (int argc, const char * argv[])
{
	CGLPixelFormatAttribute attributes[] = {
		0
	};
	CGLPixelFormatObj pixelFormat;
	GLint numberOfVirtualScreens = 0;
	if ( CGLChoosePixelFormat( attributes, &pixelFormat, &numberOfVirtualScreens ) != kCGLNoError )
	{
		fprintf( stderr, "Error choosing pixel format.\n" );
		exit( 1 );
	}
	
	CGLContextObj context;
	if ( CGLCreateContext( pixelFormat, NULL, &context ) != kCGLNoError )
	{
		fprintf( stderr, "Error creating CGL context\n" );
		exit( 1 );
	}
	
	CGLDestroyPixelFormat( pixelFormat );
	
	CGLSetCurrentContext( context );
	
	printf("vendor is: %s\n", glGetString( GL_VENDOR ));
	
	CGLDestroyContext( context );
	
    return 0;
}
