//
//  VNCView.h
//  VNCClient
//
//  Created by Douglas Richardson on 3/20/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.

#import <Cocoa/Cocoa.h>
#import "RemoteFrameBufferProtocol.h"

@interface VNCView : NSView
{
	RemoteFrameBufferProtocol *mRFB;
	uint8_t mMouseButtonState;
	unsigned int mCurrentModifierFlags;
	NSSound *mBellSound;
	NSString *mLastPasteboardData;
}
-(void)setRemoteFrameBufferProtocol:(RemoteFrameBufferProtocol*)newRFB;
@end
