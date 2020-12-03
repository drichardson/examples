//
//  BackgroundThread.h
//  Testing Grounds
//
//  Created by Douglas Richardson on 3/11/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface BackgroundThread : NSObject {

}
// Thread entry point routine.
+ (void)MyClassThreadMethod:(id)anObject;
- (void)MyInstanceThreadMethod:(id)onObject;
- (void)TimerEvent:(NSTimer*)theTimer;
@end
