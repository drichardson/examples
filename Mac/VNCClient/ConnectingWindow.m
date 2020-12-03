//
//  ConnectingWindow.m
//  VNCClient
//
//  Created by Douglas Richardson on 3/31/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.

#import "ConnectingWindow.h"

@implementation ConnectingWindow

static const double NUMBER_OF_STEPS = 3.0;

-(void)setRemoteFrameBufferProtocol:(RemoteFrameBufferProtocol*)newRFB
{
	if(newRFB != mRFB) {
		if([mRFB delegate] == self) [mRFB setDelegate:nil];
		[mRFB release];
		mRFB = [newRFB retain];
		[mRFB setDelegate:self];
	}
}

-(void) dealloc {
	[self setRemoteFrameBufferProtocol:nil];
	
	[super dealloc];
}

-(void)cancelConnecting
{
	[self setRemoteFrameBufferProtocol:nil];
	
	SEL newConnectionCancelled = @selector(newConnectionCancelled);
	if([[self delegate] respondsToSelector:newConnectionCancelled])
		[[self delegate] performSelector:newConnectionCancelled];
}

-(void)handleError:(NSString*)message
   informativeText:(NSString*)informativeText
{
	NSAlert *alert = [[NSAlert alloc] init];
	[alert addButtonWithTitle:@"OK"];
	[alert setMessageText:message];
	if(informativeText)
		[alert setInformativeText:informativeText];
	[alert runModal];
	[alert release];
	
	[self cancelConnecting];
}

- (IBAction)doCancel:(id)sender
{
	[self cancelConnecting];
}

-(void)initialStep:(NSString*)stepStatus
{
	// Put the progress bar back to 0.
	[progress incrementBy:-[progress doubleValue]];
	[connectingStatus setStringValue:stepStatus];
}

-(void)advanceStep:(NSString*)stepStatus
{
	double range = [progress maxValue] - [progress minValue];
	double oneStep = range / NUMBER_OF_STEPS;
	[progress incrementBy:oneStep];
	
	[connectingStatus setStringValue:stepStatus];
}

-(RemoteFrameBufferProtocol*)remoteFrameBufferProtocol { return mRFB; }

-(void)makeVisibleAndConnectTo:(NSString*)nameOrAddress
						  port:(uint16_t)port
{
	// TODO: localize
	NSString *connectingToMsg = [@"Connecting to " stringByAppendingString:nameOrAddress];
	[connectingTo setStringValue:connectingToMsg];
	[self initialStep:@"Waiting for response from server."];
	
	[self setIsVisible:YES];
	
	RemoteFrameBufferProtocol *rfb = [[RemoteFrameBufferProtocol alloc] init];
	[self setRemoteFrameBufferProtocol:rfb];
	[rfb release];
	rfb = nil;
	
	NSError *error = nil;
	BOOL isConnected = [mRFB connectToHost:nameOrAddress
									  port:port
									 error:&error];
	
	if(!isConnected) {
		// TODO: localize
		NSString *info = error ? [error localizedDescription] : nil;
		[self handleError:@"Error connecting to remote host."
		  informativeText:info];
	}
}

// Handle the RemoteFrameBufferProtocol delegate message.
-(id) rfbEvent:(enum RFBDelegateEventType)eventType
	 eventData:(NSDictionary*)eventData
{
	if(eventType == RFBEventType_SelectSecurityType) {
		// TODO: Handle other security types besides none.
		enum RFBSecurityType securityTypeToSelect = RFBSecurityType_None;
		
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
			
			if([securityType intValue] == securityTypeToSelect) {
				// TODO: localize
				[self advanceStep:@"Selected security type."];
				return securityType;
			}
		}
		
		// TODO: localize
		[self handleError:@"Server does not allow an unsecured connection."
		  informativeText:nil];
		
		return nil; // Desired security type not found.
	} else if(eventType == RFBEventType_SecurityResult) {
		// Input data is an NSNumber.
		id data = [eventData objectForKey:RFBSecurityResultKey];
		NSAssert([data isKindOfClass:[NSNumber class]],
				 @"data is not NSNumber for security result event.");
		NSNumber *securityResult = (NSNumber*)data;
		
		if([securityResult intValue] == RFBSecurityResultOK) {
			// TODO: localize
			[self advanceStep:@"Server says security result is OK"];
		} else {
			// On error, there may be a message, depending on the protocol version in use.
			data = [eventData objectForKey:RFBResultMessageKey];
			NSString *message = @"";
			if(data) {
				NSAssert([data isKindOfClass:[NSString class]],
						 @"data is not NSString for security result message.");
				message = (NSString*)data;
			}
		
			[self handleError:@"Error connecting to remote host."
			  informativeText:message];
		}
	} else if(eventType == RFBEventType_FrameBufferUpdate) {
		
		// When a frame buffer update is received, inform the delegate.
		SEL connectionComplete = @selector(connectionComplete);
		if([[self delegate] respondsToSelector:connectionComplete])
			[[self delegate] performSelector:connectionComplete];
		
		// TODO: localize
		[self advanceStep:@"Received frame buffer update. Connection complete."];
		
	} else if(eventType == RFBEventType_ConnectionClosed) {
		
		// TODO: localize
		[self handleError:@"Connection closed"
		  informativeText:[eventData objectForKey:RFBResultMessageKey]];
	}
	
	return nil;
}

@end
