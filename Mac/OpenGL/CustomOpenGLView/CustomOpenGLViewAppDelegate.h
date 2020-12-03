//
//  CustomOpenGLViewAppDelegate.h
//  CustomOpenGLView
//
//  Created by Doug Richardson on 12/29/10.
//  Copyright 2010 Doug Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface CustomOpenGLViewAppDelegate : NSObject <NSApplicationDelegate> {
    NSWindow *window;
}

@property (assign) IBOutlet NSWindow *window;

@end
