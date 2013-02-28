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
@end

#define kScaleUnit 100.0

@implementation CustomOpenGLView

- (id)initWithFrame:(NSRect)frame pixelFormat:(NSOpenGLPixelFormat*)pixelFormat
{
	self = [super initWithFrame:frame];
	
	if ( self )
	{
		_pixelFormat = pixelFormat;
		
		if ( _pixelFormat == nil )
		{
			NSLog(@"nil pixel format cannot be used to initialize a CustomOpenGLView");
			return nil;
		}
		
		_scale = kScaleUnit;
		
		_openGLContext = [[NSOpenGLContext alloc] initWithFormat:_pixelFormat shareContext:nil];
		
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(_globalFrameDidChangeNotification:) name:NSViewGlobalFrameDidChangeNotification object:self];
	}
	
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)lockFocus
{
	[super lockFocus];
	
	if ( [_openGLContext view] != self )
	{
		[_openGLContext setView:self];
		[self _update];
	}
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

- (void)drawRect:(NSRect)dirtyRect
{
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
	
	glEnd();
	glFlush(); // supposedly I shouldn't call this before flushBuffer but no worky if I don't
	
	[_openGLContext flushBuffer];
}

- (void)mouseDragged:(NSEvent *)theEvent
{	
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
	
	[self setNeedsDisplay:YES];
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
