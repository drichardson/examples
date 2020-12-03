//
//  RemoteFrameBufferProtocol.m
//  VNCClient
//
//  Created by Douglas Richardson on 3/20/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.
//

#import "RemoteFrameBufferProtocol.h"
#import <string.h>
#import <netinet/in.h>
#import <sys/types.h>
#import <sys/socket.h>
#import <regex.h>
#import <CoreFoundation/CoreFoundation.h>

NSString * const RFBResultMessageKey = @"RFBResultMessageKey";
NSString * const RFBSecurityListKey = @"RFBSecurityListKey";
NSString * const RFBSecurityResultKey = @"RFBSecurityResultKey";
NSString * const RFBDesktopNameKey = @"RFBDesktopNameKey";
NSString * const RFBFrameBufferUpdateMessageKey = @"RFBFrameBufferUpdateMessageKey";
NSString * const RFBServerCutTextKey = @"RFBServerCutTextKey";

const uint8_t RFBPointerButton1Down = 1;
const uint8_t RFBPointerButton2Down = 2;
const uint8_t RFBPointerButton3Down = 4;
const uint8_t RFBPointerButton4Down = 8;
const uint8_t RFBPointerButton5Down = 16;
const uint8_t RFBPointerButton6Down = 32;
const uint8_t RFBPointerButton7Down = 64;
const uint8_t RFBPointerButton8Down = 128;

static NSString *currentMessageTypeKey = @"currentMessageTypeKey";

@interface RFBUpdateMessage : NSObject <RemoteFrameBufferUpdate>
{
	uint16_t mXPosition, mYPosition, mWidth, mHeight;
	NSMutableData *mPixelData;
}
-(id) initWithXPosition:(uint16_t)xPosition
			  yPosition:(uint16_t)yPosition
				  width:(uint16_t)width
				 height:(uint16_t)height;

-(void)appendPixelData:(const void*)pixelData length:(unsigned int)length;
-(int)pixelBytesNeeded;
@end

@implementation RFBUpdateMessage

-(id) initWithXPosition:(uint16_t)xPosition
			  yPosition:(uint16_t)yPosition
				  width:(uint16_t)width
				 height:(uint16_t)height
{
	self = [super init];
	if(self) {
		mXPosition = xPosition;
		mYPosition = yPosition;
		mWidth = width;
		mHeight = height;
		mPixelData = [[NSMutableData alloc] init];
	}
	return self;
}

-(void)dealloc
{	
	[mPixelData release];
	mPixelData = nil;
	
	[super dealloc];
}

-(void)appendPixelData:(const void*)pixelData length:(unsigned int)length
{
	[mPixelData appendBytes:pixelData length:length];
}

-(int)pixelBytesNeeded
{
	const int BYTES_PER_PIXEL = 4;
	int bytesAtComplete = [self width] * [self height] * BYTES_PER_PIXEL;
	return bytesAtComplete - [mPixelData length];
}

-(uint16_t) xPosition { return mXPosition; }
-(uint16_t) yPosition { return mYPosition; }
-(uint16_t) width { return mWidth; }
-(uint16_t) height { return mHeight; }
-(NSData*) pixleData { return mPixelData; }

@end

@implementation RemoteFrameBufferProtocol
-(id)initWithClientInit:(enum RFBClientInitOptions)clientInitOptions
{
	self = [super init];
	if(self) {
		mProtocolState = RFBPNoConnection;
		mStageState = [[NSMutableDictionary alloc] init];
		mStageData = [[NSMutableData alloc] init];
		mInvocation = nil;
		mProtocolVersion = RFBProtocolVersion_3_3;
		mSecurityType = RFBSecurityType_Invalid;
		mClientInitOptions = clientInitOptions;
		mDesktopName = nil;
		mFrameBufferWidth = 0;
		mFrameBufferHeight = 0;
		mOutputStreamStagingArea = [[NSMutableData alloc] init];
	}
	return self;
}

-(id)init
{
	return [self initWithClientInit:RFBClientInit_ExclusiveDesktopAccess];
}

-(void) setStream:(NSStream*)stream destination:(NSStream**)dest
{
	NSAssert(dest, @"setStream got a nil destination argument.");
	if(stream != *dest) {
		[*dest close];
		[*dest removeFromRunLoop:[NSRunLoop currentRunLoop]
						 forMode:NSDefaultRunLoopMode];
		[*dest release];
		*dest = [stream retain];
	}
}

-(void) setInputStream:(NSInputStream*)inputStream
{
	[self setStream:inputStream destination:&mInputStream];
}

-(void) setOutputStream:(NSOutputStream*)outputStream
{
	[self setStream:outputStream destination:&mOutputStream];
}

-(void)dealloc
{
	[self disconnect];
	
	[mStageState release];
	mStageState = nil;
	
	[mStageData release];
	mStageData = nil;
	
	[mInvocation release];
	mInvocation = nil;
	
	[mDesktopName release];
	mDesktopName = nil;
	
	[mLocalFrameBuffer release];
	mLocalFrameBuffer = nil;
	
	[mOutputStreamStagingArea release];
	mOutputStreamStagingArea = nil;
	
	[super dealloc];
}

-(uint16_t)frameBufferWidth { return mFrameBufferWidth; }
-(uint16_t)frameBufferHeight { return mFrameBufferHeight; }

-(enum RFBClientInitOptions)clientInitOptions { return mClientInitOptions; }

-(NSString*)desktopName { return mDesktopName; }
-(void)setDesktopName:(NSString*)desktopName {
	if(mDesktopName != desktopName) {
		[mDesktopName release];
		mDesktopName = [desktopName retain];
	}
}

-(NSBitmapImageRep*)localFrameBuffer { return mLocalFrameBuffer; }

-(id)delegate { return mDelegate; }
-(void)setDelegate:(id)delegate {
	mDelegate = delegate;
	[mInvocation release];
	mInvocation = nil;
	SEL selector = @selector(rfbEvent:eventData:);
	NSMethodSignature *ms = [delegate methodSignatureForSelector:selector];
	if(ms) {
		mInvocation = [[NSInvocation invocationWithMethodSignature:ms] retain];
		[mInvocation setSelector:selector];
		[mInvocation setTarget:mDelegate];
	}
}

-(id)delegateEventType:(enum RFBDelegateEventType)eventType
					data:(NSDictionary*)data
{
	// TODO: Is this the proper way to invoke a method?
	[mInvocation setArgument:&eventType atIndex:2];
	[mInvocation setArgument:&data atIndex:3];
	[mInvocation invoke];
	id retVal = nil;
	[mInvocation getReturnValue:&retVal];
	return retVal;
}

-(enum RFBProtocolVersion)protocolVersion { return mProtocolVersion; }
-(enum RFBSecurityType)securityType { return mSecurityType; }

-(BOOL)connectToInput:(NSInputStream*)inputStream
			   output:(NSOutputStream*)outputStream
				error:(NSError**)error
{
	mProtocolState = RFBPNoConnection;
	
	[self setInputStream:inputStream];
	[self setOutputStream:outputStream];
	
	NSError *e = nil;
	
	if([mInputStream streamStatus] == NSStreamStatusError) {
		e = [mInputStream streamError];
	} else if([mOutputStream streamStatus] == NSStreamStatusError) {
		e = [mOutputStream streamError];
	} else {
		mProtocolState = RFBPConnected;
		
		[mInputStream setDelegate:self];
		[mOutputStream setDelegate:self];
		
		[mInputStream scheduleInRunLoop:[NSRunLoop currentRunLoop]
								forMode:NSDefaultRunLoopMode];
		[mOutputStream scheduleInRunLoop:[NSRunLoop currentRunLoop]
								 forMode:NSDefaultRunLoopMode];
		
		[mInputStream open];
		[mOutputStream open];
	}
	
	if(error) *error = [[e retain] autorelease];
	
	return mProtocolState == RFBPConnected;
}

static BOOL isIPv4Address(NSString *addressOrHostname)
{
	const char* address = [addressOrHostname UTF8String];
	static const char* IPRegEx = "([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})";
	regex_t re;
	int rc = regcomp(&re, IPRegEx, REG_EXTENDED);
	
	if(!address) {
		NSLog(@"Got NULL for address in isIPv4Address. Returning NO.");
		return NO;
	}
	
	if(rc != 0) {
		char buf[1024];
		regerror(rc, &re, buf, sizeof(buf));
		printf("Error compiling regular expression: %s\n", buf);
		return NO;
	}
	
	const unsigned MIN_TRIPLET_VALUE = 0;
	const unsigned MAX_TRIPLET_VALUE = 255;
	const unsigned NUMBER_OF_TRIPLETS = 4;
	const unsigned MAX_TRIPLET_LEN = 3;
	const unsigned MATCH_COUNT = NUMBER_OF_TRIPLETS + 1;
	regmatch_t match[NUMBER_OF_TRIPLETS + 1];
	BOOL matchedEntireString = NO;
	BOOL valueOutOfRange = NO;
	
	if(regexec(&re, address, MATCH_COUNT, match, 0) == 0) {
		//printf("Match: rm_so=%lld, rm_eo=%lld\n", match.rm_so, match.rm_eo);
		matchedEntireString = strlen(address) == (match[0].rm_eo - match[0].rm_so);
		
		if(matchedEntireString) {
			// See if all the triplets are between 0 and 255, inclusive.
			int i;
			for(i = 1; i < NUMBER_OF_TRIPLETS + 1; ++i) {
				char buf[MAX_TRIPLET_LEN + 1];
				unsigned count = MIN(MAX_TRIPLET_LEN, match[i].rm_eo - match[i].rm_so);
				strlcpy(buf, address + match[i].rm_so, count + 1);
				buf[count] = 0;
				//printf("SO: %lld, EO: %lld, count: %d, BUF: %s\n", match[i].rm_so, match[i].rm_eo, count, buf);
				int val = atoi(buf);
				if(val < MIN_TRIPLET_VALUE || val > MAX_TRIPLET_VALUE) {
					valueOutOfRange = YES;
					break;
				}
			}
		}		
	}
	
	regfree(&re);
	
	return matchedEntireString && !valueOutOfRange;
	
}

-(BOOL)connectToHost:(NSString*)hostname
				port:(uint16_t)port
			   error:(NSError**)error
{
	BOOL isSuccessful = NO;
	
	// TODO: Use either hostname or IP address here.
	//NSHost *rfbServer = [NSHost hostWithName:hostname];
	NSHost *rfbServer = nil;
	
	if(isIPv4Address(hostname))
		rfbServer = [NSHost hostWithAddress:hostname];
	else
		rfbServer = [NSHost hostWithName:hostname];
	
	// TODO: Set TCP_NODELAY and other performance options.
	// For more inforamtion, see:
	// http://www-128.ibm.com/developerworks/linux/library/l-hisock.html?ca=dgr-lnxw01BoostSocket
	
	NSLog(@"connectToHost: calling getStreamsToHost");
	
	NSInputStream *inputStream = nil;
	NSOutputStream *outputStream = nil;
	[NSStream getStreamsToHost:rfbServer
						  port:port
				   inputStream:&inputStream
				  outputStream:&outputStream];

	NSLog(@"connectToHost: getStreamsToHost returned");
	
	if(inputStream && outputStream) {
		
		isSuccessful = [self connectToInput:inputStream
									 output:outputStream
									  error:error];
		
	}
	
	return isSuccessful;
}

-(void)disconnect
{
	[self setInputStream:nil];
	[self setOutputStream:nil];
}

-(void)sendDataInStagingArea
{
	if([mOutputStreamStagingArea length] > 0 && [mOutputStream hasSpaceAvailable]) {
		int bytesSent = [mOutputStream write:[mOutputStreamStagingArea bytes]
								   maxLength:[mOutputStreamStagingArea length]];
		
		NSAssert2(bytesSent <= [mOutputStreamStagingArea length],
				  @"sendAsManyBytesAsPossible: bytesSent (%d) > staging area length (%d)",
				  bytesSent, [mOutputStreamStagingArea length]);
		
		if(bytesSent >= [mOutputStreamStagingArea length]) {
			[mOutputStreamStagingArea setLength:0];
		} else if(bytesSent > 0) {
			uint8_t *nextBytes = (uint8_t*)[mOutputStreamStagingArea bytes];
			nextBytes += bytesSent;
			unsigned bytesLeft = [mOutputStreamStagingArea length] - bytesSent;
			NSMutableData *md = [[NSMutableData alloc] initWithBytes:nextBytes
															  length:bytesLeft];
			[mOutputStreamStagingArea release];
			mOutputStreamStagingArea = md;
		}
	}
}

-(void)sendBytes:(const void*)msg bytesToSend:(int)numbytes
{
	[mOutputStreamStagingArea appendBytes:msg length:numbytes];
	[self sendDataInStagingArea];
}

-(void)sendData:(NSData*)msg
{
	[mOutputStreamStagingArea appendData:msg];
	[self sendDataInStagingArea];
}

-(void)handleProtocolVersion
{
	// Get stage state information. If it doesn't exist, initialize it.
	const NSString *stateKey = @"ProtocolVersionStageState";
	const NSString *waitingForDataState = @"waitingForAllData";
	const NSString *completeState = @"complete";
	const NSString *state = [mStageState objectForKey:stateKey];
	if(state == nil) {
		state = completeState;
		[mStageState setObject:state forKey:stateKey];
	}
	
	// Setup the variables depending on the state.
	const int PROTOCOL_VERSION_MSGLEN = 12;
	uint8_t sockBuf[PROTOCOL_VERSION_MSGLEN];
	int bytesToRead = PROTOCOL_VERSION_MSGLEN;
	
	if([state isEqual:waitingForDataState]) {
		// Only a partial read was completed before. Setup
		// the variables to read the rest.
		NSAssert1(PROTOCOL_VERSION_MSGLEN > [mStageData length],
				  @"Stage data length %u exceeds protocol version message length",
				  [mStageData length]);

		bytesToRead = PROTOCOL_VERSION_MSGLEN - [mStageData length];
	} else {
		// This is the first read; reset the stage data.
		[mStageData setLength:0];
	}
	
	// Copy as much data as possible up to the end of the protocol version message.
	int bytesRead = [mInputStream read:sockBuf maxLength:bytesToRead];
	if(bytesRead > 0) [mStageData appendBytes:sockBuf length:bytesRead];
	
	NSAssert1(PROTOCOL_VERSION_MSGLEN >= [mStageData length],
			  @"Stage data length %u exceeds protocol version message length after socket read",
			  [mStageData length]);
	
	if([mStageData length] == PROTOCOL_VERSION_MSGLEN) {
		[mStageState setObject:completeState forKey:stateKey];

		// Got all of the data for the protocol version message.
		// Create the response.
		const uint8_t *V3_3 = (uint8_t*)"RFB 003.003\n";
		const uint8_t *V3_7 = (uint8_t*)"RFB 003.007\n";
		const uint8_t *V3_8 = (uint8_t*)"RFB 003.008\n";
		uint8_t highestProtocolVersionServerSupports[PROTOCOL_VERSION_MSGLEN];
		const uint8_t *responseMsg = V3_8;
		
		[mStageData getBytes:highestProtocolVersionServerSupports
					  length:sizeof(highestProtocolVersionServerSupports)];
		
		if(memcmp(highestProtocolVersionServerSupports, V3_3, PROTOCOL_VERSION_MSGLEN) == 0) {
			mProtocolVersion = RFBProtocolVersion_3_3;
			responseMsg = V3_3;
		} else if(memcmp(highestProtocolVersionServerSupports, V3_7, PROTOCOL_VERSION_MSGLEN) == 0) {
			mProtocolVersion = RFBProtocolVersion_3_7;
			responseMsg = V3_7;
		} else if(memcmp(highestProtocolVersionServerSupports, V3_8, PROTOCOL_VERSION_MSGLEN) == 0) {
			mProtocolVersion = RFBProtocolVersion_3_8;
			responseMsg = V3_8;
		} else {
			// If the version isn't one of the ones we have listed, then, according to
			// the standard, it must be a higher version. The highest version supported
			// by this code is 3.8, so use it.
			mProtocolVersion = RFBProtocolVersion_3_8;
			responseMsg = V3_8;
		}
		
		// Send response.
		// TODO: This could block. We could write to a queue that waits to send until it
		// can be done without blocking.
		[self sendBytes:responseMsg bytesToSend:PROTOCOL_VERSION_MSGLEN];
		mProtocolState = RFBPGotProtocolVersion;
	} else {
		[mStageState setObject:waitingForDataState forKey:stateKey];
	}
}

-(void)handleSecurityType_V3_3
{
	// Get stage state information. If it doesn't exist, initialize it.
	const NSString *stateKey = @"SecurityTypeStageState";
	const NSString *waitingForDataState = @"waitingForAllData";
	const NSString *completeState = @"complete";
	const NSString *state = [mStageState objectForKey:stateKey];
	if(state == nil) {
		state = completeState;
		[mStageState setObject:state forKey:stateKey];
	}
	
	// Setup the variables depending on the state.
	const int SECURITY_TYPE_MSGLEN = 4;
	uint8_t sockBuf[SECURITY_TYPE_MSGLEN];
	int bytesToRead = SECURITY_TYPE_MSGLEN;
	
	if([state isEqual:waitingForDataState]) {
		// Only a partial read was completed before. Setup
		// the variables to read the rest.
		NSAssert1(SECURITY_TYPE_MSGLEN > [mStageData length],
				  @"Stage data length %u exceeds security type message length",
				  [mStageData length]);
		
		bytesToRead = SECURITY_TYPE_MSGLEN - [mStageData length];
	} else {
		// This is the first read; reset the stage data.
		[mStageData setLength:0];
	}
	
	// Copy as much data as possible up to the end of the protocol version message.
	int bytesRead = [mInputStream read:sockBuf maxLength:bytesToRead];
	if(bytesRead > 0) [mStageData appendBytes:sockBuf length:bytesRead];
	
	NSAssert1(SECURITY_TYPE_MSGLEN >= [mStageData length],
			  @"Stage data length %u exceeds security type message length after socket read",
			  [mStageData length]);
	
	if([mStageData length] == SECURITY_TYPE_MSGLEN) {
		[mStageState setObject:completeState forKey:stateKey];
		
		uint32_t securityType = 0;
		[mStageData getBytes:&securityType length:sizeof(securityType)];
		securityType = ntohl(securityType);
		
		switch(securityType) {
			case RFBSecurityType_None:
				mSecurityType = RFBSecurityType_None;
				break;
			case RFBSecurityType_VNCAuthentication:
				mSecurityType = RFBSecurityType_VNCAuthentication;
				break;
			default:
				mSecurityType = RFBSecurityType_Invalid;
				break;
		}
		
		// Send response.
		uint32_t securityTypeReply = htonl(mSecurityType);
		[self sendBytes:&securityTypeReply bytesToSend:sizeof(securityTypeReply)];
		
		if(mSecurityType == RFBSecurityType_Invalid) {
			// TODO: Add localized messages here.
			const char* reason = "Server security type not supported by client.";
			[self sendBytes:reason bytesToSend:strlen(reason)];
			NSDictionary *dict = [NSDictionary dictionaryWithObject:[NSString stringWithCString:reason]
															 forKey:RFBResultMessageKey];
			[self delegateEventType:RFBEventType_HandshakeError data:dict];
		} else {
			mProtocolState = RFBPGotSecurityType;
		}
	} else {
		[mStageState setObject:waitingForDataState forKey:stateKey];
	}
}

-(void)handleSecurityType_V3_7_Onwards
{
	// Get stage state information. If it doesn't exist, initialize it.
	const NSString *stateKey = @"SecurityTypeStageState";
	const NSString *waitingForDataState = @"waitingForAllData";
	const NSString *completeState = @"complete";
	const NSString *listCountKey = @"listCount";
	const NSString *state = [mStageState objectForKey:stateKey];
	if(state == nil) {
		state = completeState;
		[mStageState setObject:state forKey:stateKey];
	}
	
	int bytesToRead = 0;
	uint8_t securityTypeListCount = 0;
	
	if([state isEqual:waitingForDataState]) {
		// Only a partial read was completed before. Setup
		// the variables to read the rest.
		
		securityTypeListCount = [[mStageState objectForKey:listCountKey] intValue];
		
		NSAssert1(securityTypeListCount > [mStageData length],
				  @"Stage data length %u exceeds security type message length",
				  [mStageData length]);
		
		bytesToRead = securityTypeListCount - [mStageData length];
	} else {
		// This is the first read; reset the stage data.
		[mStageData setLength:0];
		
		// Read the list count. No need to worry about blocking here
		// because it is only one byte and this method is called when
		// data is available to be read.
		[mInputStream read:&securityTypeListCount maxLength:1];
		[mStageState setObject:[NSNumber numberWithInt:securityTypeListCount]
						forKey:listCountKey];
		bytesToRead = securityTypeListCount;
	}
	
	// Copy as much data as possible up to the end of the protocol version message.
	const size_t MAX_SECURITY_LIST_COUNT = 255; // Since the list count is 1 byte, 255 is the max list count.
	uint8_t sockBuf[MAX_SECURITY_LIST_COUNT]; // No need to malloc, the max size is small.
	int bytesRead = [mInputStream read:sockBuf maxLength:bytesToRead];
	if(bytesRead > 0) [mStageData appendBytes:sockBuf length:bytesRead];

	// Expected length is only securityTypeListCount since the is a 1 byte
	// item count is not stored in the stage data.
	NSAssert1(securityTypeListCount >= [mStageData length],
			  @"Stage data length %u exceeds security type message length after socket read",
			  [mStageData length]);
	
	if([mStageData length] == securityTypeListCount) {
		[mStageState setObject:completeState forKey:stateKey];
		
		// Build an array of NSNumbers to pass to the delegate.
		NSMutableArray *typeList = [[NSMutableArray alloc] init];
		uint8_t *types = (uint8_t*)[mStageData mutableBytes];
		int i;
		
		for(i = 0; i < securityTypeListCount; ++i)
			[typeList addObject:[NSNumber numberWithUnsignedChar:types[i]]];
		
		NSDictionary *dict = [NSDictionary dictionaryWithObject:typeList
														 forKey:RFBSecurityListKey];
		id selectedType = [self delegateEventType:RFBEventType_SelectSecurityType data:dict];
		uint8_t securityType = [selectedType unsignedCharValue];
		
		[typeList release];
		
		switch(securityType) {
			case RFBSecurityType_None:
				mSecurityType = RFBSecurityType_None;
				break;
			case RFBSecurityType_VNCAuthentication:
				mSecurityType = RFBSecurityType_VNCAuthentication;
				break;
			default:
				mSecurityType = RFBSecurityType_Invalid;
				break;
		}
		
		// Send response.
		uint8_t securityTypeReply = mSecurityType;
		[self sendBytes:&securityTypeReply bytesToSend:sizeof(securityTypeReply)];
		
		if(mSecurityType == RFBSecurityType_Invalid) {
			// TODO: Add localized messages here.
			const char* reason = "Security type not supported by client API.";
			[self sendBytes:reason bytesToSend:strlen(reason)];
			NSDictionary *dict = [NSDictionary dictionaryWithObject:[NSString stringWithCString:reason]
															 forKey:RFBResultMessageKey];
			[self delegateEventType:RFBEventType_HandshakeError data:dict];
		} else {
			mProtocolState = RFBPGotSecurityType;
		}
	} else {
		[mStageState setObject:waitingForDataState forKey:stateKey];
	}
}

-(void)handleSecurityType
{
	if([self protocolVersion] == RFBProtocolVersion_3_3)
		[self handleSecurityType_V3_3];
	else
		[self handleSecurityType_V3_7_Onwards];
}

-(void)sendClientInit
{
	// Send the ClientInit message.
	uint8_t sharedFlag = [self clientInitOptions];
	[self sendBytes:&sharedFlag bytesToSend:sizeof(sharedFlag)];
	mProtocolState = RFBPSentClientInit;
}

-(void)handleSecurityResult
{
	// Get stage state information. If it doesn't exist, initialize it.
	const NSString *stateKey = @"SecurityResultStageState";
	const NSString *reasonLengthKey = @"ReasonLengthKey";
	
	const NSString *waitingForSecurityResult = @"waitingForSecurityResult";
	const NSString *waitingForReasonLength = @"waitingForReasonLength";
	const NSString *waitingForReason = @"waitingForReason";
	const NSString *completeState = @"complete";
	const NSString *state = [mStageState objectForKey:stateKey];
	
	if(state == nil) {
		state = waitingForSecurityResult;
		[mStageState setObject:state forKey:stateKey];
		[mStageData setLength:0];
	}
	
	if([state isEqual:waitingForSecurityResult]) {
		uint32_t securityResult = -1;
		const int SECURITY_RESULT_MSGLEN = sizeof(securityResult);
		int bytesToRead = SECURITY_RESULT_MSGLEN - [mStageData length];
		uint8_t securityResultBuf[SECURITY_RESULT_MSGLEN];
		int bytesRead = [mInputStream read:securityResultBuf maxLength:bytesToRead];
		
		NSAssert(bytesRead <= bytesToRead, @"Read more bytes than expected in handleSecurityResult");
		
		[mStageData appendBytes:securityResultBuf length:bytesRead];
		
		if([mStageData length] >= SECURITY_RESULT_MSGLEN) {
			[mStageData getBytes:&securityResult length:SECURITY_RESULT_MSGLEN];
			securityResult = ntohl(securityResult);
			
			[mStageData setLength:0];
			
			if(securityResult == RFBSecurityResultFailed) {
				state = waitingForReasonLength;
			} else {
				state = completeState;
				if(securityResult != RFBSecurityResultOK) securityResult = RFBSecurityResultInvalid;
				else {
					if([self securityType] == RFBSecurityType_None)
						mProtocolState = RFBPAuthenticated;
					else
						mProtocolState = RFBPGotOKSecurityResult;
				}
			}
			
			[mStageState setObject:[NSNumber numberWithInt:securityResult] forKey:RFBSecurityResultKey];
		}
	}
	
	if([state isEqual:waitingForReasonLength]) {
		const int REASON_LENGTH_MSGLEN = sizeof(uint32_t);
		uint8_t reasonLengthBuf[REASON_LENGTH_MSGLEN];
		int bytesToRead = REASON_LENGTH_MSGLEN - [mStageData length];
		
		NSAssert(bytesToRead > 0 && bytesToRead <= REASON_LENGTH_MSGLEN, @"Invalid reason length for security result.");
		
		int bytesRead = [mInputStream read:reasonLengthBuf maxLength:bytesToRead];
		
		if(bytesRead > 0) [mStageData appendBytes:reasonLengthBuf length:bytesRead];
		
		if([mStageData length] == REASON_LENGTH_MSGLEN) {
			state = waitingForReason;
			uint32_t reasonLength = -1;
			[mStageData getBytes:&reasonLength length:REASON_LENGTH_MSGLEN];
			reasonLength = ntohl(reasonLength);
			[mStageData setLength:0];
			[mStageState setObject:[NSNumber numberWithUnsignedInt:reasonLength] forKey:reasonLengthKey];
		}
	}
	
	if([state isEqual:waitingForReason]) {
		uint32_t reasonLength = [[mStageState objectForKey:reasonLengthKey] unsignedIntValue];
		int bytesToRead = reasonLength - [mStageData length];
		// TODO: make sure doesn't exceed max value.
		uint8_t *bytes = (uint8_t*)malloc(reasonLength);
		// TODO: check malloc
		int bytesRead = [mInputStream read:bytes maxLength:bytesToRead];
		if(bytesRead > 0) [mStageData appendBytes:bytes length:bytesRead];
		if([mStageData length] == reasonLength) {
			state = completeState;
			[mStageData getBytes:bytes length:reasonLength];
			NSString *reason = [NSString stringWithCString:(const char*)bytes length:reasonLength];
			[mStageState setObject:reason forKey:RFBResultMessageKey];
		}
		
		free(bytes);
		bytes = 0;
	}
	
	if([state isEqual:completeState])
		[self delegateEventType:RFBEventType_SecurityResult data:mStageState];
	
	[mStageState setObject:state forKey:stateKey];
	
	if(mProtocolState == RFBPAuthenticated)
		[self sendClientInit];
}

-(void)handleAuthentication
{	
	// TODO: Implement authentication.
	NSAssert(false, @"handleAuthentication is not implemented");
	
	// Receive 16 byte challenge.
	
	// Encrypt challenge with DES, using a password supplied by the user.
	
	// Send the response.
	
	// Send client init.
	[self sendClientInit];
}

struct PixelFormat
{
	uint8_t bitsPerPixel;
	uint8_t depth;
	uint8_t bigEndianFlag;
	uint8_t trueColorFlag;
	uint16_t redMax;
	uint16_t greenMax;
	uint16_t bluemax;
	uint8_t redShift;
	uint8_t greenShift;
	uint8_t blueShift;
	uint8_t padding[3];
};

-(void)setPixelFormat
{
	uint8_t messageType = 0;
	uint8_t padding[3];
	struct PixelFormat fmt;
	memset(&fmt, 0, sizeof(fmt));
	fmt.bitsPerPixel = 32;
	fmt.depth = 24;
	fmt.bigEndianFlag = 0; // use little endian
	fmt.trueColorFlag = 1; // use true color
	fmt.redMax = htons(0xff);
	fmt.greenMax = htons(0xff);
	fmt.bluemax = htons(0xff);
	fmt.redShift = 0;
	fmt.greenShift = 8;
	fmt.blueShift = 16;
	fmt.padding[0] = fmt.padding[1] = fmt.padding[2] = 0;
	
	NSMutableData *data = [NSMutableData data];
	[data appendBytes:&messageType length:sizeof(messageType)];
	[data appendBytes:padding length:sizeof(padding)];
	
	[data appendBytes:&fmt.bitsPerPixel length:sizeof(fmt.bitsPerPixel)];
	[data appendBytes:&fmt.depth length:sizeof(fmt.depth)];
	[data appendBytes:&fmt.bigEndianFlag length:sizeof(fmt.bigEndianFlag)];
	[data appendBytes:&fmt.trueColorFlag length:sizeof(fmt.trueColorFlag)];
	[data appendBytes:&fmt.redMax length:sizeof(fmt.redMax)];
	[data appendBytes:&fmt.greenMax length:sizeof(fmt.greenMax)];
	[data appendBytes:&fmt.bluemax length:sizeof(fmt.bluemax)];
	[data appendBytes:&fmt.redShift length:sizeof(fmt.redShift)];
	[data appendBytes:&fmt.greenShift length:sizeof(fmt.greenShift)];
	[data appendBytes:&fmt.blueShift length:sizeof(fmt.blueShift)];
	[data appendBytes:fmt.padding length:sizeof(fmt.padding)];
	
	[self sendData:data];
}

-(void)frameBufferUpdateRequest:(BOOL)incrementalUpdate
{
	const uint8_t FRAME_BUFFER_UPDATE_REQUEST_MESSAGE_TYPE = 3;
	uint8_t messageType = FRAME_BUFFER_UPDATE_REQUEST_MESSAGE_TYPE;
	uint8_t incremental = incrementalUpdate ? 1 : 0;
	uint16_t xPosition = 0;
	uint16_t yPosition = 0;
	uint16_t width = htons([self frameBufferWidth]);
	uint16_t height = htons([self frameBufferHeight]);
	
	NSMutableData *data = [NSMutableData data];
	
	[data appendBytes:&messageType length:sizeof(messageType)];
	[data appendBytes:&incremental length:sizeof(incremental)];
	[data appendBytes:&xPosition length:sizeof(xPosition)];
	[data appendBytes:&yPosition length:sizeof(yPosition)];
	[data appendBytes:&width length:sizeof(width)];
	[data appendBytes:&height length:sizeof(height)];
	
	NSLog(@"sending frameBufferUpdateRequest");
	[self sendData:data];
}

-(void)sendKeyEvent:(enum RFBKeyEventKeyState)state
			 keysym:(uint32_t)keysym
{
	NSLog(@"sendKeyEvent called");
	
	const unsigned KEYDOWN_EVENT_MESSAGE_TYPE_LENGTH = 8;
	uint8_t message[KEYDOWN_EVENT_MESSAGE_TYPE_LENGTH];
	const uint8_t KEYDOWN_EVENT_MESSAGE_TYPE = 4;
	
	keysym = htonl(keysym);
	
	message[0] = KEYDOWN_EVENT_MESSAGE_TYPE;
	message[1] = state;
	message[2] = message[3] = 0; // padding
	memcpy(message + 4, &keysym, 4);
	
	[self sendBytes:message bytesToSend:KEYDOWN_EVENT_MESSAGE_TYPE_LENGTH];	
}

-(void)sendPointerEvent:(uint8_t)downButtonMask
			  xPosition:(uint16_t)xPosition
			  yPosition:(uint16_t)yPosition
{
	NSLog(@"sendPointerEvent called");
	
	xPosition = htons(xPosition);
	yPosition = htons(yPosition);
	
	const unsigned POINTER_EVENT_MESSAGE_TYPE_LENGTH = 6;
	uint8_t message[POINTER_EVENT_MESSAGE_TYPE_LENGTH];
	const uint8_t POINTER_EVENT_MESSAGE_TYPE = 5;
	
	message[0] = POINTER_EVENT_MESSAGE_TYPE;
	message[1] = downButtonMask;
	memcpy(message + 2, &xPosition, 2);
	memcpy(message + 4, &yPosition, 2);
	
	[self sendBytes:message bytesToSend:POINTER_EVENT_MESSAGE_TYPE_LENGTH];

}

-(void)sendPaste:(NSString*)textToPaste
{
	NSLog(@"sendPaste called");
	
	const unsigned MSGLEN_FIXED_PART = 8;
	uint32_t textLength = htonl([textToPaste length]);
	const unsigned totalLength = MSGLEN_FIXED_PART + [textToPaste length];
	const uint8_t CLIENTCUTTEXT_MESSAGE_TYPE = 6;
	uint8_t *message = (uint8_t*)malloc(totalLength);
	
	if(message) {
		message[0] = CLIENTCUTTEXT_MESSAGE_TYPE;
		message[1] = message[2] = message[3] = 0; // padding
		memcpy(message + 4, &textLength, sizeof(textLength));
		memcpy(message + 8, [textToPaste UTF8String], [textToPaste length]);
		[self sendBytes:message bytesToSend:totalLength];
		
		free(message);
	} else {
		NSLog(@"Error allocating memory for paste message.");
	}
}

-(void)setupLocalFrameBuffer
{
	// The size of the frame buffer on the server is now available.
	// Create a bitmap to keep the frame buffer on the client side.
	[mLocalFrameBuffer release];
	mLocalFrameBuffer =
		[[NSBitmapImageRep alloc] initWithBitmapDataPlanes:nil
												pixelsWide:[self frameBufferWidth]
												pixelsHigh:[self frameBufferHeight]
											 bitsPerSample:8
										   samplesPerPixel:3
												  hasAlpha:NO
												  isPlanar:NO
											colorSpaceName:NSCalibratedRGBColorSpace
											   bytesPerRow:0
											  bitsPerPixel:32];
	
	// Initialize the local frame buffer with red.
	uint32_t *p = (uint32_t*)[mLocalFrameBuffer bitmapData];
	int i;
	const int pixelCount = [self frameBufferWidth] * [self frameBufferHeight];
	for(i = 0; i < pixelCount; ++i) {
		p[i] = 0xff000000;
	}
}

-(void)handleServerInit
{
	// Get stage state information. If it doesn't exist, initialize it.
	const NSString *stateKey = @"ServerInitStageState";
	const NSString *waitingForFixedData = @"waitingForFixedData";
	const NSString *waitingForVariableData = @"waitingForVariableData";
	const NSString *completeState = @"complete";
	const NSString *state = [mStageState objectForKey:stateKey];
	const int SERVER_INIT_FIXED_PART_MSGLEN = 24;
	
	if(state == nil) {
		[mStageData setLength:0];
		state = waitingForFixedData;
	}

	if([state isEqual:waitingForFixedData]) {
		int bytesLeft = SERVER_INIT_FIXED_PART_MSGLEN - [mStageData length];
		uint8_t buf[SERVER_INIT_FIXED_PART_MSGLEN];
		int bytesRead = [mInputStream read:buf maxLength:bytesLeft];
		if(bytesRead > 0) [mStageData appendBytes:buf length:bytesRead];
		if([mStageData length] == SERVER_INIT_FIXED_PART_MSGLEN) state = waitingForVariableData;
	}
	
	if([state isEqual:waitingForVariableData]) {
		uint32_t nameLength = 0;
		const int NAME_LENGTH_OFFSET = 20;
		[mStageData getBytes:&nameLength
					   range:NSMakeRange(NAME_LENGTH_OFFSET, sizeof(nameLength))];
		nameLength = ntohl(nameLength);
		// TODO: Check for results that are too big.
		int bytesLeft = nameLength - ([mStageData length] - SERVER_INIT_FIXED_PART_MSGLEN);
		uint8_t *buf = (uint8_t*)malloc(nameLength);
		// TODO: check memory alloc
		int bytesRead = [mInputStream read:buf maxLength:bytesLeft];
		if(bytesRead > 0) [mStageData appendBytes:buf length:bytesRead];
		if([mStageData length] == SERVER_INIT_FIXED_PART_MSGLEN + nameLength) {
			state = completeState;
			
			// Pull the pertinent information from the server init message.
			uint16_t tmp = 0;
			[mStageData getBytes:&tmp range:NSMakeRange(0, 2)];
			mFrameBufferWidth = ntohs(tmp);
			[mStageData getBytes:&tmp range:NSMakeRange(2, 2)];
			mFrameBufferHeight = ntohs(tmp);
			
			[mStageData getBytes:buf range:NSMakeRange(SERVER_INIT_FIXED_PART_MSGLEN, nameLength)];
			
			NSString *desktopName = [NSString stringWithCString:(const char*)buf length:nameLength];
			[self setDesktopName:desktopName];
			NSDictionary *dict = [NSDictionary dictionaryWithObject:desktopName
															 forKey:RFBDesktopNameKey];
			
			[self setupLocalFrameBuffer];
			
			[self delegateEventType:RFBEventType_ServerInit data:dict];
			mProtocolState = RFBPNormalOperation;
		}
		free(buf);
		buf = 0;
	}
	
	[mStageState setObject:state forKey:stateKey];
	
	if([state isEqual:completeState]) {
		// Inform the server of the pixel format this code supports.
		[self setPixelFormat];
		
		// For the first update request, to a non-incremental. That is,
		// have the server send the entires contents of the screen.
		[self frameBufferUpdateRequest:NO];
	}
}

-(void)clearCurrentMessage
{
	[mStageState removeObjectForKey:currentMessageTypeKey];
}

-(void)updateLocalFrameBuffer:(RFBUpdateMessage*)rfbUpdate
{
	// Cast away const qualifier. It doesn't matter if the pixel data in the update
	// message is changed, it won't be used again.
	unsigned char *srcPixelData = (unsigned char*)[[rfbUpdate pixleData] bytes];		
	uint16_t xPos = [rfbUpdate xPosition];
	uint16_t yPos = [rfbUpdate yPosition];
	uint16_t localWidth = [self frameBufferWidth];
	uint16_t srcHeight = [rfbUpdate height];
	uint16_t srcWidth = [rfbUpdate width];
	unsigned char *srcRowBase;
	unsigned char *destRowBase;
	unsigned char *destPixelData = [mLocalFrameBuffer bitmapData];
	const int PIXEL_BYTES = 4;
	
	// Copy the source pixels (from the update message) to the local frame
	// buffer one scan line at a time.
	int row = 0;
	for(row = 0; row < srcHeight; ++row) {
		srcRowBase = srcPixelData + ((srcWidth * row) * PIXEL_BYTES);
		destRowBase = destPixelData + ((((yPos + row) * localWidth) + xPos) * PIXEL_BYTES);
		memcpy(destRowBase, srcRowBase, srcWidth * PIXEL_BYTES);
	}
}

-(void)handleFrameBufferUpdate
{
	NSString *stateKey = @"FBUStateKey";
	NSString *stateNumberOfRectanglesKey = @"FBURectCount";
	NSString *stateRectanglesReceivedKey = @"FBURectanglesReceived";
	
	NSString *fixedPart = @"waitingForFixedData";
	NSString *readRectangleHeader = @"ReadRectangleHeader";
	NSString *readPixelData = @"ReadPixelData";
	NSString *complete = @"complete";
	NSString *state = [mStageState objectForKey:stateKey];
	
	if(state == nil)
		state = complete;
	
	if([state isEqual:complete]) {
		// The last handler completed. Reset the state data.
		[mStageState setObject:[NSNumber numberWithInt:0] forKey:stateNumberOfRectanglesKey];
		[mStageState setObject:[NSNumber numberWithInt:0] forKey:stateRectanglesReceivedKey];
		[mStageData setLength:0];
		state = fixedPart;
		NSLog(@"RBU: Handling NEW frameBufferUpdateRequest");
	} else
		NSLog(@"RBU: Handling other part.");
	
	if([state isEqual:fixedPart]) {
		const int FIXED_PART_LENGTH = 3;
		uint8_t fixedPartBuffer[FIXED_PART_LENGTH];
		int bytesToRead = FIXED_PART_LENGTH - [mStageData length];
		int bytesRead = [mInputStream read:fixedPartBuffer maxLength:bytesToRead];
		
		if(bytesRead > 0) [mStageData appendBytes:fixedPartBuffer length:bytesRead];
		
		if([mStageData length] == FIXED_PART_LENGTH) {
			uint16_t numberOfRectangles = 0;
			[mStageData getBytes:&numberOfRectangles range:NSMakeRange(1, sizeof(numberOfRectangles))];
			[mStageData setLength:0];
			numberOfRectangles = ntohs(numberOfRectangles);
			[mStageState setObject:[NSNumber numberWithUnsignedShort:numberOfRectangles]
															  forKey:stateNumberOfRectanglesKey];
			
			NSLog([NSString stringWithFormat:@"RBU: Going to process %u rectangles", numberOfRectangles]);
			
			if(numberOfRectangles > 0)
				state = readRectangleHeader;
			else
				state = complete;
		}
	}
	
	if([state isEqual:readRectangleHeader] || [state isEqual:readPixelData]) {
		BOOL isDataAvailable = YES;
		int currentRectangle = [[mStageState objectForKey:stateRectanglesReceivedKey] intValue];
		int rectangleCount = [[mStageState objectForKey:stateNumberOfRectanglesKey] intValue];
		
		while(currentRectangle < rectangleCount && isDataAvailable) {
		
			if([state isEqual:readRectangleHeader]) {
				int RECTANGLE_HEADER_LEN = 12;
				uint8_t rectangleHeader[RECTANGLE_HEADER_LEN];
				
				int bytesToRead = RECTANGLE_HEADER_LEN - [mStageData length];
				int bytesRead = [mInputStream read:rectangleHeader maxLength:bytesToRead];
				if(bytesRead > 0)
					[mStageData appendBytes:rectangleHeader length:bytesRead];
				else
					isDataAvailable = NO;
				
				if([mStageData length] == RECTANGLE_HEADER_LEN) {
					uint16_t xPosition, yPosition, width, height;
					int32_t encodingType;
					
					[mStageData getBytes:&xPosition range:NSMakeRange(0, 2)];
					xPosition = ntohs(xPosition);
					
					[mStageData getBytes:&yPosition range:NSMakeRange(2, 2)];
					yPosition = ntohs(yPosition);
					
					[mStageData getBytes:&width range:NSMakeRange(4, 2)];
					width = ntohs(width);
					
					[mStageData getBytes:&height range:NSMakeRange(6, 2)];
					height = ntohs(height);
					
					[mStageData getBytes:&encodingType range:NSMakeRange(8, 4)];
					encodingType = ntohl(encodingType);
					
					[mStageData setLength:0];
					NSLog([NSString stringWithFormat:@"RBU: encodingType is %d", encodingType]);
					
					RFBUpdateMessage* rfbUpdateMessage = [[RFBUpdateMessage alloc] initWithXPosition:xPosition
																						   yPosition:yPosition
																							   width:width
																							  height:height];
					
					[mStageState setObject:rfbUpdateMessage forKey:RFBFrameBufferUpdateMessageKey];
					
					[rfbUpdateMessage release];
					
					state = readPixelData;
				}
			}
			
			if([state isEqual:readPixelData]) {
				RFBUpdateMessage* rfbUpdateMessage = [mStageState objectForKey:RFBFrameBufferUpdateMessageKey];
				NSAssert(rfbUpdateMessage, @"rfbUpdateMessage not in stage state of readPixelData");
				
				uint8_t *pixelData = (uint8_t*)malloc([rfbUpdateMessage pixelBytesNeeded]);
				int bytesRead = [mInputStream read:pixelData maxLength:[rfbUpdateMessage pixelBytesNeeded]];
				if(bytesRead > 0)
					[rfbUpdateMessage appendPixelData:pixelData length:bytesRead];
				else
					isDataAvailable = NO;
				
				free(pixelData);
				pixelData = 0;
				
				if([rfbUpdateMessage pixelBytesNeeded] == 0) {
					[self updateLocalFrameBuffer:rfbUpdateMessage];
					NSLog(@"RBU: Received entire rectangle - calling delegate");
					[self delegateEventType:RFBEventType_FrameBufferUpdate data:mStageState];
					NSLog(@"RBU: Delegate returned");
					++currentRectangle;
					[mStageState setObject:[NSNumber numberWithInt:currentRectangle]
									forKey:stateRectanglesReceivedKey];
					
					if(currentRectangle == rectangleCount)
						state = complete;
					else
						state = readRectangleHeader;
				}
			}
		}
	}
	
	if([state isEqual:complete]) {
		// As soon as processing completes for the current update request, issue another.
		NSLog(@"RBU: Completed handling frameBufferUpdateRequest");
		[self clearCurrentMessage];
		[self frameBufferUpdateRequest:YES];
	}
	
	[mStageState setObject:state forKey:stateKey];
}

-(void)handleSetColorMapEntries
{
	// TODO: Read the server message and do nothing with it.
	NSLog(@"ERROR - Received SetColorMapEntries message but I don't implement it!");
	[self clearCurrentMessage];
}

-(void)handleBell
{
	[self delegateEventType:RFBEventType_RingBell data:nil];
	[self clearCurrentMessage];
}

-(void)handleServerCutText
{
	NSString *stateKey = @"SCTStateKey";
	NSString *stateTextLengthKey = @"SCTTextLength";
	NSString *stateTextReceivedKey = @"SCTTextReceived";
	
	NSString *fixedPart = @"waitingForFixedData";
	NSString *readText = @"readText";
	NSString *complete = @"complete";
	NSString *state = [mStageState objectForKey:stateKey];
	
	if(state == nil)
		state = complete;
	
	if([state isEqual:complete]) {
		// The last handler completed. Reset the state data.
		[mStageState setObject:[NSNumber numberWithInt:0] forKey:stateTextLengthKey];
		[mStageState setObject:[NSNumber numberWithInt:0] forKey:stateTextReceivedKey];
		[mStageData setLength:0];
		state = fixedPart;
		NSLog(@"SCT: Handling NEW frameBufferUpdateRequest");
	} else
		NSLog(@"SCT: Handling other part.");
	
	if([state isEqual:fixedPart]) {
		const int FIXED_PART_LENGTH = 7;
		uint8_t fixedPartBuffer[FIXED_PART_LENGTH];
		int bytesToRead = FIXED_PART_LENGTH - [mStageData length];
		int bytesRead = [mInputStream read:fixedPartBuffer maxLength:bytesToRead];
		
		if(bytesRead > 0) [mStageData appendBytes:fixedPartBuffer length:bytesRead];
		
		if([mStageData length] == FIXED_PART_LENGTH) {
			uint32_t textLength = 0;
			[mStageData getBytes:&textLength range:NSMakeRange(3, sizeof(textLength))];
			[mStageData setLength:0];
			textLength = ntohl(textLength);
			[mStageState setObject:[NSNumber numberWithUnsignedShort:textLength]
							forKey:stateTextLengthKey];
			
			NSLog([NSString stringWithFormat:@"SCT: Going to process %u bytes of text", textLength]);
			
			if(textLength > 0)
				state = readText;
			else
				state = complete;
		}
	}
	
	if([state isEqual:readText]) {
		BOOL isDataAvailable = YES;
		int currentText = [[mStageState objectForKey:stateTextReceivedKey] intValue];
		int textLength = [[mStageState objectForKey:stateTextLengthKey] intValue];
		
		while(currentText < textLength && isDataAvailable) {
			int bytesLeft = textLength - currentText;
			uint8_t *textData = (uint8_t*)malloc(bytesLeft);
			int bytesRead = [mInputStream read:textData maxLength:bytesLeft];
			if(bytesRead > 0)
				[mStageData appendBytes:textData length:bytesRead];
			else
				isDataAvailable = NO;
			
			free(textData);
			textData = 0;
			
			if(bytesLeft == bytesRead) {
				NSLog(@"SCT: Received all text data - calling delegate");
				NSString *text = [NSString stringWithCString:[mStageData bytes] length:[mStageData length]];
				[mStageData setLength:0];
				NSDictionary *dict = [NSDictionary dictionaryWithObject:text
																 forKey:RFBServerCutTextKey];
				[self delegateEventType:RFBEventType_ServerCutText data:dict];
				
				state = complete;
			}
			
			currentText += bytesRead;
		}
	}
	
	if([state isEqual:complete]) {
		NSLog(@"SCT: Completed handling a server cut text message.");
		[self clearCurrentMessage];
	}
	
	[mStageState setObject:state forKey:stateKey];
}

enum RFBServerMessage {
	FrameBufferUpdate = 0,
	SetColorMapEntries = 1,
	RingBell = 2,
	ServerCutText = 3
};

-(void)handleNormalOperation
{
	NSNumber *currentMessageType = [mStageState objectForKey:currentMessageTypeKey];
	
	if(currentMessageType == nil) {
		int bytesRead = 0;
		uint8_t messageType = 0;
		bytesRead = [mInputStream read:&messageType maxLength:sizeof(messageType)];
		if(bytesRead == sizeof(messageType)) {
			currentMessageType = [NSNumber numberWithInt:messageType];
			[mStageState setObject:currentMessageType forKey:currentMessageTypeKey];
		}
	}
	
	if(currentMessageType) {
		switch([currentMessageType unsignedCharValue]) {
			case FrameBufferUpdate:
				[self handleFrameBufferUpdate];
				break;
			case SetColorMapEntries:
				[self handleSetColorMapEntries];
				break;
			case RingBell:
				[self handleBell];
				break;
			case ServerCutText:
				[self handleServerCutText];
				break;
			default:
				NSAssert1(false,
						  @"Invalid current message type of %d",
						  [currentMessageType unsignedCharValue]);
				break;
		}
	}
}

-(void)handleServerMessage
{	
	switch(mProtocolState) {
		case RFBPConnected:
			[self handleProtocolVersion];
			break;
		case RFBPGotProtocolVersion:
			[self handleSecurityType];
			break;
		case RFBPGotSecurityType:
			[self handleSecurityResult];
			break;
		case RFBPGotOKSecurityResult:
			[self handleAuthentication];
			break;
		case RFBPSentClientInit:
			[self handleServerInit];
			break;
		case RFBPNormalOperation:
			[self handleNormalOperation];
			break;
		default:
			NSLog(@"handleServerMessage: got unexpected protocol state.");
			break;
	}
}

-(void)stream:(NSStream *)stream handleEvent:(NSStreamEvent)eventCode
{
	switch(eventCode) {
		case NSStreamEventNone: // No event has occurred.
			NSLog(@"handleEvent: NSStreamEventNone");
			break;
		case NSStreamEventOpenCompleted: // The open has completed successfully.
			NSLog(@"handleEvent: NSStreamEventOpenCompleted");
			break;
		case NSStreamEventHasBytesAvailable: // The stream has bytes to be read.
			//NSLog(@"handleEvent: NSStreamEventHasBytesAvailable");
			// TODO: Do I need to check that stream is mInputStream here?
			[self handleServerMessage];
			break;
		case NSStreamEventHasSpaceAvailable: // The stream can accept bytes for writing.
			//NSLog(@"handleEvent: NSStreamEventHasSpaceAvailable");
			[self sendDataInStagingArea];
			break;
		case NSStreamEventErrorOccurred: // An error has occurred on the stream.
		{
			NSLog(@"handleEvent: NSStreamEventErrorOccurred");
			
			NSString *closedReason = nil;
			
			if(stream == mInputStream)
				closedReason = @"Error occurred on input stream."; // TODO: Localize message.
			else if(stream == mOutputStream)
				closedReason = @"Error occurred on output stream."; // TODO: Localize message.
				
				if(closedReason) {
					[self setInputStream:nil];
					[self setOutputStream:nil];
					
					NSDictionary *dict = [NSDictionary dictionaryWithObject:closedReason
																	 forKey:RFBResultMessageKey];
					
					[self delegateEventType:RFBEventType_ConnectionClosed data:dict];
				}
					break;			
			
			break;
		}
		case NSStreamEventEndEncountered: // The end of the stream has been reached.
		{
			NSLog(@"handleEvent: NSStreamEventEndEncountered");
			
			// TODO: Add back in if needed
			NSString *closedReason = nil;
			
			if(stream == mInputStream)
				closedReason = @"Input stream was closed."; // TODO: Localize message.
			else if(stream == mOutputStream)
				closedReason = @"Output stream was closed."; // TODO: Localize message.
			
			if(closedReason) {
				[self setInputStream:nil];
				[self setOutputStream:nil];
				
				NSDictionary *dict = [NSDictionary dictionaryWithObject:closedReason
																 forKey:RFBResultMessageKey];
				
				[self delegateEventType:RFBEventType_ConnectionClosed data:dict];
			}
			break;
		}
	}
}
@end
