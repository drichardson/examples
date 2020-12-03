//
//  TextureTestAppDelegate.m
//  TextureTest
//
//  Created by Doug on 4/27/11.
//  Copyright 2011 Doug Richardson. All rights reserved.
//

#import "TextureTestAppDelegate.h"

@implementation TextureTestAppDelegate

@synthesize window, glView;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
}

- (IBAction)simpleTexture:(id)sender
{
    glView.usePNG = NO;
}

- (IBAction)pngTexture:(id)sender
{
    glView.usePNG = YES;
}

@end
