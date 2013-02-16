//
//  MyOpenGLLayer.m
//  Layers
//
//  Created by Doug on 7/13/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import "MyOpenGLLayer.h"
#import <OpenGL/OpenGL.h>


@implementation MyOpenGLLayer

-(BOOL)canDrawInCGLContext:(CGLContextObj)glContext pixelFormat:(CGLPixelFormatObj)pixelFormat forLayerTime:(CFTimeInterval)timeInterval displayTime:(const CVTimeStamp *)timeStamp
{
	// Just like the default, we'll just always return YES and always refresh.
	// You normally would not override this method to do this.
	return YES;
}

- (void)drawInCGLContext:(CGLContextObj)glContext
			 pixelFormat:(CGLPixelFormatObj)pixelFormat
			forLayerTime:(CFTimeInterval)timeInterval
			 displayTime:(const CVTimeStamp *)timeStamp
{
	CGLSetCurrentContext(glContext);
	
	float rotation = timeInterval * 10; // 10 Degrees per second.
	
	glClear(GL_COLOR_BUFFER_BIT);
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	glRotatef(rotation, 0, 0, 1);
	
	glBegin(GL_QUADS);
	glColor3f(0.0, 1.0, 0.0);
	glVertex2f(-0.5, -0.5);
	glVertex2f(-0.5, 0.5);
	glVertex2f(0.5, 0.5);
	glVertex2f(0.5, -0.5);
	glEnd();
	
	glPopMatrix();
	
	[super drawInCGLContext:glContext pixelFormat:pixelFormat forLayerTime:timeInterval displayTime:timeStamp];
}

@end
