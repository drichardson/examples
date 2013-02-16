//
//  ChildWindowAppDelegate.h
//  ChildWindow
//
//  Created by Doug on 9/16/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface ChildWindowAppDelegate : NSObject <NSApplicationDelegate> {
    NSWindow *window;
}

@property (assign) IBOutlet NSWindow *window;

@end
