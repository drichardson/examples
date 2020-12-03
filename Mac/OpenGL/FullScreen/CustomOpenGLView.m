//
//  CustomOpenGLView.m
//  CustomOpenGLView
//
//  Created by Doug Richardson on 12/29/10.
//  Copyright 2010 Doug Richardson. All rights reserved.
//

#import "CustomOpenGLView.h"


@implementation CustomOpenGLView

- (id)initWithFrame:(NSRect)frame pixelFormat:(NSOpenGLPixelFormat*)pixelFormat
{
	self = [super initWithFrame:frame];
	
	if ( self )
	{
		_pixelFormat = [pixelFormat retain];
		
		if ( _pixelFormat == nil )
		{
			NSLog(@"nil pixel format cannot be used to initialize a CustomOpenGLView");
			[self release];
			return nil;
		}
		
		_openGLContext = [[NSOpenGLContext alloc] initWithFormat:_pixelFormat shareContext:nil];
		
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(_globalFrameDidChangeNotification:) name:NSViewGlobalFrameDidChangeNotification object:self];
	}
	
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[_pixelFormat release];
	[_openGLContext release];
	
	[super dealloc];
}

- (void)lockFocus
{
	[super lockFocus];
	
	if ( [_openGLContext view] != self )
		[_openGLContext setView:self];
}

- (void)drawRect:(NSRect)dirtyRect
{
	[_openGLContext makeCurrentContext];
	
	// Perform drawing here
	glClearColor( 1, 0, 0, 1 );
	glClear( GL_COLOR_BUFFER_BIT );
	
	glColor3f(1, .85, .35);
	glBegin(GL_TRIANGLES);
	{
		glVertex3f(0, 0.6, 0);
		glVertex3f(-0.2, -0.3, 0);
		glVertex3f(.2, -.3, 0);
	}
	glEnd();
	glFlush(); // supposedly I shouldn't call this before flushBuffer but no worky if I don't
	
	[_openGLContext flushBuffer];
}

- (void)update
{
	[_openGLContext update];
}

- (void)_globalFrameDidChangeNotification:(NSNotification*)notification
{
	[self update];
}

@end
