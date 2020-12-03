//
//  CustomOpenGLViewAppDelegate.h
//  CustomOpenGLView
//
//  Created by Doug Richardson on 12/29/10.
//  Copyright 2010 Doug Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "CustomOpenGLView.h"

@interface CustomOpenGLViewAppDelegate : NSObject <NSApplicationDelegate>

@property (strong) IBOutlet NSWindow *window;
@property (strong) IBOutlet CustomOpenGLView *glView;

@end
