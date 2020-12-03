//
//  CustomOpenGLView.m
//  CustomOpenGLView
//
//  Created by Doug Richardson on 12/29/10.
//  Copyright 2010 Doug Richardson. All rights reserved.
//

#import "CustomOpenGLView.h"
#import <OpenGL/OpenGL.h>
#import <OpenGL/glu.h>

@interface CustomOpenGLView ()
- (void)_update;
- (void)_drawScene;
@end

#define kScaleUnit 100.0

@implementation CustomOpenGLView
{
    NSOpenGLContext* _openGLContext;
	
	GLfloat _xTranslation, _yTranslation, _zTranslation;
	GLfloat _rotation;
	GLfloat _scale;
    
    CVDisplayLinkRef _displayLink;
}

- (id)initWithFrame:(NSRect)frame
{
	self = [super initWithFrame:frame];
	
	if ( self )
	{
		_scale = kScaleUnit;
	}
	
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
    CVDisplayLinkRelease(_displayLink);
}

- (void)viewWillMoveToWindow:(NSWindow *)newWindow
{
    assert([NSThread isMainThread]);
    
    if ( newWindow == nil )
    {
        [self _teardownOpenGL];
    }
}

- (void)setDoubleBuffer:(NSNumber *)doubleBuffer
{
    [self willChangeValueForKey:@"doubleBuffer"];
    _doubleBuffer = doubleBuffer;
    [self didChangeValueForKey:@"doubleBuffer"];
    
    // Re-create the opengl context since this affects the pixel format.
    [self _teardownOpenGL];
    [self _prepareOpenGL];
}

- (void)setSyncToVerticalRetrace:(NSNumber *)syncToVerticalRetrace
{
    [self willChangeValueForKey:@"syncToVerticalRetrace"];
    syncToVerticalRetrace = syncToVerticalRetrace;
    [self didChangeValueForKey:@"syncToVerticalRetrace"];
    
    GLint val = [syncToVerticalRetrace boolValue] ? 1 : 0;
    [_openGLContext setValues:&val forParameter:NSOpenGLCPSwapInterval];
}

- (void)_teardownOpenGL
{
    [[NSNotificationCenter defaultCenter] removeObserver:self name:NSViewGlobalFrameDidChangeNotification object:self];
    
    CVDisplayLinkStop(_displayLink);
    CVDisplayLinkRelease(_displayLink);
    _displayLink = NULL;
}

- (void)_prepareOpenGL
{
    assert([NSThread isMainThread]);
    
    NSOpenGLPixelFormatAttribute attributesSingleBuffer[] = {
        
        // setup for anti-aliasing
        NSOpenGLPFASampleBuffers, 1,
        NSOpenGLPFASamples, 4,
        // 0 terminated
        0
    };
    
    NSOpenGLPixelFormatAttribute attributesDoubleBuffer[] = {
        
        // setup for anti-aliasing
        NSOpenGLPFASampleBuffers, 1,
        NSOpenGLPFASamples, 4,
        NSOpenGLPFADoubleBuffer,
        // 0 terminated
        0
    };
    
    NSOpenGLPixelFormatAttribute* attributes = [_doubleBuffer boolValue] ? attributesDoubleBuffer : attributesSingleBuffer;
    
    NSOpenGLPixelFormat* pixelFormat = [[NSOpenGLPixelFormat alloc] initWithAttributes:attributes];
    
    _openGLContext = [[NSOpenGLContext alloc] initWithFormat:pixelFormat shareContext:nil];
    GLint val = [_syncToVerticalRetrace boolValue] ? 1 : 0;
    [_openGLContext setValues:&val forParameter:NSOpenGLCPSwapInterval];
    [_openGLContext setView:self];
    [self _update];
    
    
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(_globalFrameDidChangeNotification:) name:NSViewGlobalFrameDidChangeNotification object:self];
    
    
    // Let display link drive frame updates.
    CVReturn rc = CVDisplayLinkCreateWithActiveCGDisplays(&_displayLink);
    
    if ( rc != kCVReturnSuccess )
    {
        NSLog(@"ERROR: Couldn't create display link. %d", rc);
    }
    
    CVDisplayLinkSetOutputCallback(_displayLink, DisplayLinkCallback, (__bridge void *)(self));
    
    rc = CVDisplayLinkSetCurrentCGDisplayFromOpenGLContext(_displayLink, [_openGLContext CGLContextObj], [pixelFormat CGLPixelFormatObj]);
    if ( rc != kCVReturnSuccess )
    {
        NSLog(@"ERROR: Couldn't set current CG display from OpenGL context. %d", rc);
    }
    
    CVDisplayLinkStart(_displayLink);
}

- (void)viewDidMoveToWindow
{
    [self _prepareOpenGL];
}

- (void)_drawForTime:(const CVTimeStamp*)time
{
    // WARNING: This is NOT called from the main thread.
    assert(![NSThread isMainThread]);
    [self _drawScene];
}

// This is the renderer output callback function
static CVReturn DisplayLinkCallback(CVDisplayLinkRef displayLink, const CVTimeStamp* now, const CVTimeStamp* outputTime, CVOptionFlags flagsIn, CVOptionFlags* flagsOut, void* displayLinkContext)
{
    [(__bridge CustomOpenGLView*)displayLinkContext _drawForTime:outputTime];
    return kCVReturnSuccess;
}

static inline void drawIsoscelesTriangle(GLfloat legLength)
{
	glBegin(GL_TRIANGLES);
	{
		// counter-clockwise
		glVertex3f(0, 0, 0);
		glVertex3f(legLength, 0, 0);
		glVertex3f(legLength / 2.0, legLength, 0);
	}
	glEnd();
}

- (void)_drawScene
{
    assert(![NSThread isMainThread]);
    
	[_openGLContext makeCurrentContext];
	
	float currentViewport[] = { -1, -1, -1, -1, -1, -1 };
	glGetFloatv( GL_VIEWPORT, currentViewport );
	
	glClearColor( 1, 0, 0, 1 );
	glClear( GL_COLOR_BUFFER_BIT );
	
	glMatrixMode( GL_MODELVIEW );
	glLoadIdentity();
	
	glColor3f(1, .85, .35);
	glTranslatef( _xTranslation, -_yTranslation, 0 );
	glScalef( _scale / kScaleUnit, _scale / kScaleUnit, 0 );
	glRotatef( _rotation, 0, 0, 1 );
	drawIsoscelesTriangle( 40 );
	
    if ( [_doubleBuffer boolValue] )
    {
        // Automatically calls glFlush.
        [_openGLContext flushBuffer];
    }
    else
    {
        glFlush();
    }
}

- (void)mouseDragged:(NSEvent *)theEvent
{
    assert([NSThread isMainThread]);
    
	NSUInteger modifierFlags = [theEvent modifierFlags];	
	
	if ( modifierFlags & NSCommandKeyMask )
	{
		_rotation += [theEvent deltaX];
	}
	else if ( modifierFlags & NSAlternateKeyMask )
	{
		_scale += [theEvent deltaX];
	}
	else
	{
		_xTranslation += [theEvent deltaX];
		_yTranslation += [theEvent deltaY];
	}
}

- (void)_update
{
	[_openGLContext update];
	
	NSRect bounds = [self bounds];
	[_openGLContext makeCurrentContext];
	
	// Set the viewport to be the entire bounds
	glViewport( 0, 0, bounds.size.width, bounds.size.height );
	
	// Make the projection use units that correspond to pixels (otherwise the content would be scaled).
	glMatrixMode( GL_PROJECTION );
	glLoadIdentity();
	gluOrtho2D(0, bounds.size.width, 0, bounds.size.height);
	
	// Cull backwards (clockwise) facing polygons.
	glCullFace( GL_BACK );
	glEnable( GL_CULL_FACE );
	
	// Use anti-aliasing for lines
	glEnable( GL_POINT_SMOOTH );
	glEnable( GL_LINE_SMOOTH );
	glEnable( GL_POLYGON_SMOOTH );
	glEnable( GL_BLEND );
	glEnable( GL_ALPHA_TEST );
	glEnable( GL_MULTISAMPLE );
	glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
	glLineWidth( 1.5 );
}

- (void)_globalFrameDidChangeNotification:(NSNotification*)notification
{
	[self _update];
}

@end
