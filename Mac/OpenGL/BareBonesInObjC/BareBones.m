#import <Foundation/Foundation.h>
#import <OpenGL/OpenGL.h>
#import <Cocoa/Cocoa.h>
#import <OpenGL/gl.h>

int main (int argc, const char * argv[])
{
    @autoreleasepool {
        NSOpenGLPixelFormatAttribute attributes[] = {
            0
        };
        
        NSOpenGLPixelFormat* pixelFormat = [[NSOpenGLPixelFormat alloc] initWithAttributes:attributes];
        NSOpenGLContext* context = [[NSOpenGLContext alloc] initWithFormat:pixelFormat shareContext:nil];
        pixelFormat = nil;
        
        
        [context makeCurrentContext];
        printf("vendor is: %s\n", glGetString( GL_VENDOR ));
    }
	
    return 0;
}
