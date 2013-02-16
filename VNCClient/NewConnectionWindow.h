//
//  NewConnectionWindow.h
//  VNCClient
//
//  Created by Douglas Richardson on 3/30/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.

#import <Cocoa/Cocoa.h>
#import "RemoteFrameBufferProtocol.h"
#import "ConnectingWindow.h"

@interface NewConnectionWindow : NSWindow
{
    IBOutlet ConnectingWindow *connectingWindow;
    IBOutlet NSTextField *mAddress;
    IBOutlet NSTextField *mPort;
}
- (IBAction)doCancel:(id)sender;
- (IBAction)doConnect:(id)sender;

-(RemoteFrameBufferProtocol*)remoteFrameBufferProtocol;

-(void)connectionComplete;
-(void)newConnectionCancelled;
@end
