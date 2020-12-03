//
//  main.m
//  SimpleProxy
//
//  Created by Douglas Richardson on 2/11/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "SPLog.h"

int main(int argc, char *argv[])
{
    @autoreleasepool
    {
        dispatch_queue_t logQueue = dispatch_queue_create("simpleproxy.log.queue", NULL);
        SPLogInitialize(LOG_DEBUG, [NSSet set], ^(NSString *msg) {
            // Could make this dispatch_async, but then logs won't go out when I step over a line while
            // debugging.
            dispatch_sync(logQueue, ^{
                NSLog(@"%@", msg); 
            });
        });
    }
    
    return NSApplicationMain(argc, (const char **)argv);
}
