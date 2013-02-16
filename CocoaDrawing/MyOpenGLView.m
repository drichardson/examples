#import "MyOpenGLView.h"
#import <OpenGL/gl.h>
#import <GLUT/glut.h>

@implementation MyOpenGLView

- (void)prepareOpenGL
{
	puts("prepareOpenGL");
	glOrtho(0,1,0,1,-1,1); // Specify the coordinate system OpenGl uses to draw the final image.
}

- (void)drawRect:(NSRect)rect
{
	puts("MyOpenGLView drawRect");
	
	// Set the view to black.
	glClearColor(0, 0, 0, 0);
	glClear(GL_COLOR_BUFFER_BIT);

	glColor3f(1,0,0); // Set the shape color to white.
	
	// Draw some shapes.
	glBegin(GL_POLYGON);
	glVertex3f(0.25, 0.25, 0);
	glVertex3f(0.75, 0.25, 0);
	glVertex3f(0.75, 1.75, 0);
	glVertex3f(0.25, 0.75 , 0);
	glEnd();
	
	glColor3f(0,1,0);
	glBegin(GL_POLYGON);
	glVertex3f(0.15, 0.15, -1);
	glVertex3f(0.65, 0.15, -1);
	glVertex3f(0.65, 0.65, -1);
	glVertex3f(0.15, 0.65 , -1);
	glEnd();
	
	// Flush to ensure all drawing commands are executed.
	glFlush();	
}

@end
