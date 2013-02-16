//
//  VNCViewWindow.h
//  VNCClient
//
//  Created by Douglas Richardson on 3/30/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.

#import <Cocoa/Cocoa.h>
#import "VNCView.h"
#import "RemoteFrameBufferProtocol.h"

@interface VNCViewWindow : NSWindow
{
    IBOutlet VNCView *vncView;
}

-(void)setRemoteFrameBufferProtocol:(RemoteFrameBufferProtocol*)newRFB;

@end
