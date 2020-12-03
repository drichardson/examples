//
//  NewConnectionWindow.m
//  VNCClient
//
//  Created by Douglas Richardson on 3/30/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.

#import "NewConnectionWindow.h"

@implementation NewConnectionWindow

// Delegate message for ConnectingWindow
-(void)connectionComplete
{
	[connectingWindow setIsVisible:NO];
	
	SEL connectionComplete = @selector(connectionComplete);
	if([[self delegate] respondsToSelector:connectionComplete])
		[[self delegate] performSelector:connectionComplete];
}

// Delegate message for ConnectingWindow
-(void)newConnectionCancelled
{
	[connectingWindow setIsVisible:NO];
	[self setIsVisible:YES];
}

- (IBAction)doCancel:(id)sender
{
	SEL newConnectionCancelled = @selector(newConnectionCancelled);
	if([[self delegate] respondsToSelector:newConnectionCancelled])
		[[self delegate] performSelector:newConnectionCancelled];
}

- (IBAction)doConnect:(id)sender
{	
	[self setIsVisible:NO];
	[connectingWindow makeVisibleAndConnectTo:[mAddress stringValue]
										 port:[mPort intValue]];
}

-(RemoteFrameBufferProtocol*)remoteFrameBufferProtocol
{
	return [connectingWindow remoteFrameBufferProtocol];
}

@end
