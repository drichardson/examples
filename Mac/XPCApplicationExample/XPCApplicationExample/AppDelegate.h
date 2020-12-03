//
//  AppDelegate.h
//  XPCApplicationExample
//
//  Created by Douglas Richardson on 3/3/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface AppDelegate : NSObject <NSApplicationDelegate>

@property (assign) IBOutlet NSWindow *window;
@property (assign) IBOutlet NSTextField* xField;
@property (assign) IBOutlet NSTextField* yField;
@property (assign) IBOutlet NSTextField* resultField;

- (IBAction)enterPressed:(id)sender;

@end
