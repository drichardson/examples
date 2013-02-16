#import <Foundation/Foundation.h>
#import <OpenGL/OpenGL.h>
#import <Cocoa/Cocoa.h>

int main (int argc, const char * argv[])
{
	NSAutoreleasePool* pool = [NSAutoreleasePool new];
	
	NSOpenGLPixelFormatAttribute attributes[] = {
		0
	};
	
	NSOpenGLPixelFormat* pixelFormat = [[NSOpenGLPixelFormat alloc] initWithAttributes:attributes];
	NSOpenGLContext* context = [[NSOpenGLContext alloc] initWithFormat:pixelFormat shareContext:nil];
	[pixelFormat release];
	pixelFormat = nil;
	
	
	[context makeCurrentContext];
	printf("vendor is: %s\n", glGetString( GL_VENDOR ));
	
	[context release];
	
	[pool release];
	
    return 0;
}
