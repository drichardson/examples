//
//  AppDelegate.m
//  XPCApplicationExample
//
//  Created by Douglas Richardson on 3/3/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import "AppDelegate.h"

@implementation AppDelegate

@synthesize window = _window, xField=_xField, yField=_yField, resultField=_resultField;

- (void)dealloc
{
    [super dealloc];
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    // Insert code here to initialize your application
}

- (IBAction)enterPressed:(id)sender
{
    double x = [_xField doubleValue];
    double y = [_yField doubleValue];
    NSLog(@"x = %f, y = %f", x, y);
    
    xpc_connection_t conn = xpc_connection_create("com.doug.XPCMultiplierService", NULL);
    xpc_object_t message = xpc_dictionary_create(NULL, NULL, 0);
    // You have to set an event handler or else xpc_connection_resume will crash.
    xpc_connection_set_event_handler(conn, ^(xpc_object_t object) {
    });
    xpc_connection_resume(conn);
    
    xpc_dictionary_set_double(message, "x", x);
    xpc_dictionary_set_double(message, "y", y);
    xpc_connection_send_message_with_reply(conn, message, dispatch_get_main_queue(), ^(xpc_object_t object) {
        double result = xpc_dictionary_get_double(object, "result");
        NSLog(@"Result is %f", result);
        [_resultField setDoubleValue:result];
    });
    xpc_release(message);
    xpc_release(conn);
}

@end
