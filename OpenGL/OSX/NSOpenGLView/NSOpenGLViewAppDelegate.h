//
//  NSOpenGLViewAppDelegate.h
//  NSOpenGLView
//
//  Created by Doug Richardson on 12/29/10.
//  Copyright 2010 Doug Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface NSOpenGLViewAppDelegate : NSObject <NSApplicationDelegate> {
    NSWindow *window;
}

@property (assign) IBOutlet NSWindow *window;

@end
