//
//  AppDelegate.h
//  CoreAnimationblending
//
//  Created by Douglas Richardson on 4/23/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface AppDelegate : NSObject <NSApplicationDelegate>

@property (assign) IBOutlet NSWindow *window;

- (IBAction)image:(id)sender;
- (IBAction)fastBlur:(id)sender;
- (IBAction)coreImageFilter:(id)sender;
- (IBAction)overlay:(id)sender;
- (IBAction)partialBlurRect:(id)sender;
- (IBAction)fastPartialBlurRect:(id)sender;
- (IBAction)mask:(id)sender;
- (IBAction)partialBlurOnVideo:(id)sender;

@end
