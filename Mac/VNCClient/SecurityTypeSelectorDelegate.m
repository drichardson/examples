//
//  SecurityTypeSelectorDelegate.m
//  VNCClient
//
//  Created by Douglas Richardson on 3/23/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.
//

#import "SecurityTypeSelectorDelegate.h"

@implementation SecurityTypeSelectorDelegate

-(enum RFBSecurityResult)securityResult { return mSecurityResult; }

-(NSString*)desktopName { return mDesktopName; }

-(void)setDesktopName:(NSString*)desktopName
{
	if(desktopName != mDesktopName) {
		[mDesktopName release];
		mDesktopName = [desktopName copy];
	}
}

-(id <RemoteFrameBufferUpdate, NSObject>) remoteFrameBufferUpdate { return mRemoteFrameBufferUpdate; }

-(id) initWithSecurityType:(enum RFBSecurityType) securityType
{
	self = [super init];
	if(self) {
		mSecurityTypeToSelect = securityType;
		mSecurityResult = RFBSecurityResultFailed;
		mDesktopName = nil;
	}
	return self;
}

-(void) dealloc
{
	[self setDesktopName:nil];
	
	[super dealloc];
}

-(id) rfbEvent:(enum RFBDelegateEventType)eventType eventData:(NSDictionary*)eventData
{
	if(eventType == RFBEventType_SelectSecurityType) {
		// Input data is an NSArray of numbers. Return
		// the number that should be selected or nil
		// if none can be handled.
		id data = [eventData objectForKey:RFBSecurityListKey];
		NSAssert([data isKindOfClass:[NSArray class]],
				 @"data is not NSArray for security type event.");
		NSArray *securityTypeList = (NSArray*)data;
		NSEnumerator *e = [securityTypeList objectEnumerator];
		id obj = nil;
		while((obj = [e nextObject])) {
			NSAssert([obj isKindOfClass:[NSNumber class]],
					 @"Member of security type list was not an NSNumber");
			NSNumber *securityType = (NSNumber*)obj;
			
			if([securityType intValue] == mSecurityTypeToSelect) 
				return securityType;
		}
		
		return nil; // Desired security type not found.
	} else if(eventType == RFBEventType_SecurityResult) {
		// Input data is an NSNumber.
		id data = [eventData objectForKey:RFBSecurityResultKey];
		NSAssert([data isKindOfClass:[NSNumber class]],
				 @"data is not NSNumber for security result event.");
		NSNumber *securityResult = (NSNumber*)data;
		
		mSecurityResult = [securityResult intValue];
		
		// On error, there may be a message, depending on the protocol version in use.
		data = [eventData objectForKey:RFBResultMessageKey];
		if(data) {
			NSAssert([data isKindOfClass:[NSString class]],
					 @"data is not NSString for security result message.");
			NSString *message = (NSString*)data;
			NSLog(message);
		}
	} else if(eventType == RFBEventType_ServerInit) {
		[self setDesktopName:[eventData objectForKey:RFBDesktopNameKey]];
	} else if(eventType == RFBEventType_FrameBufferUpdate) {
		id <RemoteFrameBufferUpdate, NSObject> rfbUpdate = [eventData objectForKey:RFBFrameBufferUpdateMessageKey];
		if(rfbUpdate != mRemoteFrameBufferUpdate) {
			[mRemoteFrameBufferUpdate release];
			mRemoteFrameBufferUpdate = [rfbUpdate retain];
		}
	}
	
	return nil;
}

@end
