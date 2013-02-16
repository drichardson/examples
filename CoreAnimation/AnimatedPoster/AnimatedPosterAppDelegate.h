//
//  AnimatedPosterAppDelegate.h
//  AnimatedPoster
//
//  Created by Doug on 11/4/10.
//  Copyright 2010 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface AnimatedPosterAppDelegate : NSObject <NSApplicationDelegate> {
    NSWindow *window;
}

@property (assign) IBOutlet NSWindow *window;

@end
