//
//  MyOpenGLView.m
//  NSOpenGLView
//
//  Created by Doug Richardson on 12/29/10.
//  Copyright 2010 Doug Richardson. All rights reserved.
//

#import "MyOpenGLView.h"


@implementation MyOpenGLView


- (void)drawRect:(NSRect)rect
{
	glClearColor(0, 0, 0, 0);
	glClear(GL_COLOR_BUFFER_BIT);
	
	glColor3f(1, .85, .35);
	glBegin(GL_TRIANGLES);
	{
		glVertex3f(0, 0.6, 0);
		glVertex3f(-0.2, -0.3, 0);
		glVertex3f(.2, -.3, 0);
	}
	glEnd();
	
	glFlush();
}

@end
