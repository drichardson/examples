//
//  PhysicsSimulationAppDelegate.m
//  PhysicsSimulation
//
//  Created by Doug Richardson on 4/11/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "PhysicsSimulationAppDelegate.h"
#import <QuartzCore/QuartzCore.h>
#import <CoreVideo/CoreVideo.h>
#import "Simulation.h"

@interface PhysicsSimulationAppDelegate ()
@property (assign) CGDirectDisplayID mainWindowDisplayID;
@property (assign) CVDisplayLinkRef displayLink;
@property (retain) Simulation* simulation;
@end

const double kWorldWidth = 100.0;
const double kWorldHeight = 100.0;


static CVReturn DisplayLinkCallback( CVDisplayLinkRef displayLink, const CVTimeStamp *inNow, const CVTimeStamp *inOutputTime, CVOptionFlags flagsIn, CVOptionFlags *flagsOut, void *displayLinkContext);



@implementation PhysicsSimulationAppDelegate

@synthesize window, glView, mainWindowDisplayID, displayLink, simulation;

- (void)_updateViewport
{
    // Setup the gl viewport
    double width = kWorldWidth;
    double height = kWorldHeight;
    NSRect frame = [window frame];
    [[glView openGLContext] makeCurrentContext];
    glViewport(0, 0, frame.size.width, frame.size.height);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(-width/2.0, width/2.0, -height/2.0, height/2.0, 0, 1);
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    NSNotificationCenter* nc = [NSNotificationCenter defaultCenter];
    [nc addObserver:self selector:@selector(windowDidChangeScreen:) name:NSWindowDidChangeScreenNotification object:window];
    [nc addObserver:self selector:@selector(windowDidResize:) name:NSWindowDidResizeNotification object:window];
    
    [self _updateViewport];
    
   // glEnable(GL_POLYGON_SMOOTH);
    //glEnable(GL_ALPHA_TEST);
   // glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    //glEnable(GL_BLEND);
    glEnable(GL_MULTISAMPLE);
    
    // Setup the simulation update timer.
    const uint64_t kSimulationUpdatesPerSecond = 100;
    uint64_t interval = NSEC_PER_SEC / kSimulationUpdatesPerSecond;
    uint64_t leeway = interval / 4;
    
    dispatch_queue_t queue = dispatch_queue_create("SimulationQueue", NULL);
    dispatch_set_target_queue(queue, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0));
    dispatch_source_t timer = dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, 0, queue);
    dispatch_source_set_timer(timer, DISPATCH_TIME_NOW, interval, leeway);
    
    dispatch_source_set_event_handler(timer, ^(void) {
        [simulation update];
    });
    
    dispatch_resume(timer);
}

- (void)dealloc {
    
    if ( displayLink )
    {
        CVDisplayLinkRelease(displayLink);
    }
    
    self.simulation = nil;
    
    [super dealloc];
}

- (void)updateDisplayLink
{
    CGDirectDisplayID displayID = (CGDirectDisplayID)[[window.screen.deviceDescription objectForKey:@"NSScreenNumber"] intValue];
    
    if ( displayID != kCGNullDirectDisplay && displayID != mainWindowDisplayID )
    {
        mainWindowDisplayID = displayID;
        
        if ( displayLink == NULL )
        {
            CVReturn err = CVDisplayLinkCreateWithCGDisplay(mainWindowDisplayID, &displayLink);
            
            if ( err != kCVReturnSuccess )
            {
                NSLog(@"Error creating display link for display ID %d, err=%d", mainWindowDisplayID, err);
            }
            
            err = CVDisplayLinkSetOutputCallback(displayLink, DisplayLinkCallback, self);
            
            if ( err != kCVReturnSuccess )
            {
                NSLog(@"Error setting display link output callback. %d", err);
            }
            
            err = CVDisplayLinkStart(displayLink);
            
            if ( err != kCVReturnSuccess )
            {
                NSLog(@"Error starting display link.");
            }
        }
        else
        {
            CVReturn err = CVDisplayLinkSetCurrentCGDisplay(displayLink, mainWindowDisplayID);
            
            if ( err != kCVReturnSuccess )
            {
                NSLog(@"Error setting display link to new display %d", mainWindowDisplayID);
            }
        }
    }
}

- (void)awakeFromNib
{
    simulation = [[Simulation alloc] initWithWorldRect:CGRectMake(-kWorldWidth/2.0, -kWorldHeight/2.0, kWorldWidth, kWorldHeight)];
    [self updateDisplayLink];
}

- (void)windowDidChangeScreen:(NSNotification*)inNotification
{
    [self updateDisplayLink];
}

- (void)windowDidResize:(NSNotification*)notification
{
    updateViewport = YES;
}

- (void)displayFrame
{
    // NOTE: This is NOT run on the main thread.
    [[glView openGLContext] makeCurrentContext];
    
    if ( updateViewport )
    {
        dispatch_sync(dispatch_get_main_queue(), ^(void) {
            updateViewport = NO;
            [self _updateViewport];
        });
    }
    
    [simulation render];
}

@end


static CVReturn DisplayLinkCallback( CVDisplayLinkRef displayLink, const CVTimeStamp *inNow, const CVTimeStamp *inOutputTime, CVOptionFlags flagsIn, CVOptionFlags *flagsOut, void *displayLinkContext)
{
    // From documentation:
    // Your callback must retrieve the frame with the timestamp specified by the (inOutputTime parameter, manipulate it if desired (for example, apply color correction or map into onto a surface), and then output it to the display.
    
    // Also note, this is not called from the main thread.
    NSAutoreleasePool* pool = [NSAutoreleasePool new];
    [(PhysicsSimulationAppDelegate*)displayLinkContext displayFrame];
    [pool release];
    return kCVReturnSuccess;
}
