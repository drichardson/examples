//
//  VNCViewWindow.m
//  VNCClient
//
//  Created by Douglas Richardson on 3/30/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.

#import "VNCViewWindow.h"

@implementation VNCViewWindow

-(void)setRemoteFrameBufferProtocol:(RemoteFrameBufferProtocol*)newRFB
{
	[vncView setRemoteFrameBufferProtocol:newRFB];
}

@end
