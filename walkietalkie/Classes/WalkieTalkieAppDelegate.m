//
//  WalkieTalkieAppDelegate.m
//  WalkieTalkie
//
//  Created by Doug on 1/22/09.
//  Copyright Douglas Richardson 2009. All rights reserved.
//

#import "WalkieTalkieAppDelegate.h"
#import "WalkieTalkieViewController.h"

@implementation WalkieTalkieAppDelegate

@synthesize window;
@synthesize viewController;


- (void)applicationDidFinishLaunching:(UIApplication *)application {    
    
    // Override point for customization after app launch    
    [window addSubview:viewController.view];
    [window makeKeyAndVisible];
}


- (void)dealloc {
    [viewController release];
    [window release];
    [super dealloc];
}


@end
