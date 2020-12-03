//
//  SecurityTypeSelectorDelegate.h
//  VNCClient
//
//  Created by Douglas Richardson on 3/23/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "RemoteFrameBufferProtocol.h"

@interface SecurityTypeSelectorDelegate : NSObject
{
	enum RFBSecurityType mSecurityTypeToSelect;
	enum RFBSecurityResult mSecurityResult;
	NSString *mDesktopName;
	id <RemoteFrameBufferUpdate, NSObject> mRemoteFrameBufferUpdate;
}

-(id) initWithSecurityType:(enum RFBSecurityType) securityType;

-(enum RFBSecurityResult)securityResult;
-(NSString*)desktopName;
-(id <RemoteFrameBufferUpdate, NSObject>) remoteFrameBufferUpdate;
@end

