//
//  SPAppDelegate.h
//  SimpleProxy
//
//  Created by Douglas Richardson on 2/11/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "SPProxyServer.h"

@interface SPAppDelegate : NSObject <NSApplicationDelegate, SPServerDelegate>

@property (assign) IBOutlet NSWindow *window;

@end
