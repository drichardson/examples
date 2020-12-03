//
//  VNCWindowController.m
//  VNCClient
//
//  Created by Douglas Richardson on 3/30/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.

#import "VNCWindowController.h"

@implementation VNCWindowController

- (IBAction)closeVNCViewWindow:(id)sender
{
	[vncViewWindow setIsVisible:NO];
	[vncViewWindow setRemoteFrameBufferProtocol:nil];
	mWasConnected = NO;
}

- (IBAction)showNewConnectionWindow:(id)sender
{	
	[vncViewWindow setIsVisible:NO];
	[vncViewWindow setRemoteFrameBufferProtocol:nil];
	[newConnectionWindow setIsVisible:YES];
}

-(void)connectionComplete
{
	[newConnectionWindow setIsVisible:NO];
	RemoteFrameBufferProtocol *rfb = [newConnectionWindow remoteFrameBufferProtocol];
	[vncViewWindow setRemoteFrameBufferProtocol:rfb];
	
	[vncViewWindow setTitle:[@"Connected to " stringByAppendingString:[rfb desktopName]]];
	
	[vncViewWindow setContentSize:NSMakeSize([rfb frameBufferWidth], [rfb frameBufferHeight])];
	[vncViewWindow center];
	[vncViewWindow setIsVisible:YES];
	mWasConnected = YES;
}

-(void)newConnectionCancelled
{
	[newConnectionWindow setIsVisible:NO];
	mWasConnected = NO;
}

- (IBAction)showVNCViewWindow:(id)sender
{
	[vncViewWindow setIsVisible:YES];
}

- (BOOL)validateUserInterfaceItem:(id <NSValidatedUserInterfaceItem>)anItem
{
	SEL sel = [anItem action];
	BOOL v = YES;
	
	if(sel == @selector(closeVNCViewWindow:))
		v = mWasConnected;
	else if(sel == @selector(showNewConnectionWindow:))
		v = ![newConnectionWindow isVisible];
	else if(sel == @selector(showVNCViewWindow:))
		v = ![vncViewWindow isVisible] && mWasConnected;
	
	return v;
}

@end
