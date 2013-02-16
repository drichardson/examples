//
//  ConnectingWindow.h
//  VNCClient
//
//  Created by Douglas Richardson on 3/31/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.
#import <Cocoa/Cocoa.h>
#import "RemoteFrameBufferProtocol.h"

@interface ConnectingWindow : NSWindow
{
    IBOutlet NSTextField *connectingStatus;
    IBOutlet NSTextField *connectingTo;
    IBOutlet NSProgressIndicator *progress;
	RemoteFrameBufferProtocol *mRFB;
}
- (IBAction)doCancel:(id)sender;

-(void)makeVisibleAndConnectTo:(NSString*)nameOrAddress
			port:(uint16_t)port;

-(RemoteFrameBufferProtocol*)remoteFrameBufferProtocol;
@end
