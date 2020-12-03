//
//  BackgroundThread.m
//  Testing Grounds
//
//  Created by Douglas Richardson on 3/11/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "BackgroundThread.h"


@implementation BackgroundThread

+ (void)MyClassThreadMethod:(id)anObject
{
	// Autorelease pool required before any Cocoa objects are created
	// in this thread.
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	
	puts("MyClassThreadMethod called. Running run loop.");
	
	[[NSRunLoop currentRunLoop] run];
	
	puts("MyClasssThreadMethod. Run loop ended.");
	
	[pool release];
}

- (void)MyInstanceThreadMethod:(id)anObject
{
	// Autorelease pool required before any Cocoa objects are created
	// in this thread.
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	
	puts("MyInstanceThreadMethod called. Running run loop.");
	
	[NSTimer scheduledTimerWithTimeInterval:2.0 target:self selector:@selector(TimerEvent:) userInfo:nil repeats:YES];
	[[NSRunLoop currentRunLoop] run];
	
	puts("MyInstanceThreadMethod. Run loop ended.");
	
	[pool release];
}

- (void)TimerEvent:(NSTimer*)theTimer
{
	static int i = 0;
	++i;
	printf("Got timer event %d\n", i);
}
@end
