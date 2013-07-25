//
//  MyOpenGLView.m
//  TextureTest
//
//  Created by Doug on 4/27/11.
//  Copyright 2011 Doug Richardson. All rights reserved.
//

#import "MyOpenGLView.h"
#import <OpenGL/gl.h>

@implementation MyOpenGLView

@synthesize usePNG;

- (void)_loadSimpleTexture
{
    glPixelStoref(GL_UNPACK_ALIGNMENT, 1);
    
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
    
    GLbyte pixels[3*4] = {
        255, 0, 0,
        0, 255, 0,
        0, 0, 255,
        40, 40, 40,
    };
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 2, 2, 0, GL_RGB, GL_UNSIGNED_BYTE, pixels);
}

- (void)_loadPNGTexture
{
    glPixelStoref(GL_UNPACK_ALIGNMENT, 1);
    
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
    
    NSImage* image = [NSImage imageNamed:@"Texture1.png"];
    NSBitmapImageRep* imageRep = [[image representations] objectAtIndex:0];
    
    assert([imageRep bitsPerPixel] == 32);
    assert([imageRep samplesPerPixel] == 3);
    
    GLbyte* pixels = (GLbyte*)[imageRep bitmapData];
    
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, image.size.width, image.size.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, pixels);
}

- (void)_draw
{
    [[self openGLContext] makeCurrentContext];
    
    NSRect bounds = self.bounds;
    
    glClearColor(0, 0, 0, 1);
    glClear(GL_COLOR_BUFFER_BIT);
    
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    
    glOrtho(NSMinX(bounds), NSMaxX(bounds), NSMinY(bounds), NSMaxY(bounds), 0, 1);
    glEnable(GL_TEXTURE_2D);
    
    glColor3f(0, 1, 0);
    
    GLuint texture;
    glGenTextures(1, &texture);
    
    glBindTexture(GL_TEXTURE_2D, texture);
    
    if ( usePNG )
    {
        [self _loadPNGTexture];
    }
    else
    {
        [self _loadSimpleTexture];
    }
    
    glScalef(0.8, 0.8, 1);
    glTranslatef(40, 40, 0);
    
    glBegin(GL_QUADS);
    glTexCoord2i(0, 0);
    glVertex2f(NSMinX(bounds), NSMaxY(bounds));
    glTexCoord2i(1, 0);
    glVertex2f(NSMaxX(bounds), NSMaxY(bounds));
    glTexCoord2i(1, 1);
    glVertex2f(NSMaxX(bounds), NSMinY(bounds));
    glTexCoord2i(0, 1);
    glVertex2f(NSMinX(bounds), NSMinY(bounds));
    glEnd();
    
    glDeleteTextures(1, &texture);
    
    glFlush();
    
#if DEBUG
    GLenum err = glGetError();
    if ( err != GL_NO_ERROR )
    {
        NSLog( @"GL error 0x%X", err);
    }
#endif
}

- (void)drawRect:(NSRect)dirtyRect
{
    [self _draw];
}

- (void)setUsePNG:(BOOL)newUsePNG
{
    usePNG = newUsePNG;
    [self _draw];
}

@end
