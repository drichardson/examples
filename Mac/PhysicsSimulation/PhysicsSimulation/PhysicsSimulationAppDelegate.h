//
//  PhysicsSimulationAppDelegate.h
//  PhysicsSimulation
//
//  Created by Doug Richardson on 4/11/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface PhysicsSimulationAppDelegate : NSObject <NSApplicationDelegate> {
    BOOL updateViewport;
}

@property (assign) IBOutlet NSWindow* window;
@property (assign) IBOutlet NSOpenGLView* glView;

@end
