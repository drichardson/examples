//
//  TextureTestAppDelegate.h
//  TextureTest
//
//  Created by Doug on 4/27/11.
//  Copyright 2011 Doug Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <OpenGL/OpenGL.h>
#import "MyOpenGLView.h"

@interface TextureTestAppDelegate : NSObject <NSApplicationDelegate> {
@private
    NSWindow *window;
    MyOpenGLView *glView;
    
}

@property (assign) IBOutlet NSWindow *window;
@property (assign) IBOutlet MyOpenGLView *glView;

- (IBAction)simpleTexture:(id)sender;
- (IBAction)pngTexture:(id)sender;

@end
