//
//  TestRemoteFrameBufferProtocol.m
//  VNCClient
//
//  Created by Douglas Richardson on 3/21/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.
//

#import "TestRemoteFrameBufferProtocol.h"
#import "InjectableInputStream.h"
#import "ReadableOutputStream.h"
#import "SecurityTypeSelectorDelegate.h"
#import <arpa/inet.h>

@implementation TestRemoteFrameBufferProtocol

-(void)setUp
{
	NSLog(@"setUp Called");
	mRFB = [[RemoteFrameBufferProtocol alloc] init];
}

-(void)tearDown
{
	NSLog(@"tearDown Called");
	[mRFB release];
	mRFB = nil;
}

static const unsigned PROTOCOL_VERSION_MSGLEN = 12;

-(void)doConnectInjectableInputStream:(InjectableInputStream**)inputStreamOut
				 readableOutputStream:(ReadableOutputStream**)outputStreamOut
{
	InjectableInputStream *is = [[[InjectableInputStream alloc] init] autorelease];
	ReadableOutputStream *os = [[[ReadableOutputStream alloc] init] autorelease];
	
	STAssertTrue([mRFB connectToInput:is output:os error:nil], @"connectToInput failed");
		
	if(inputStreamOut) *inputStreamOut = is;
	if(outputStreamOut) *outputStreamOut = os;
}

-(void)doProtocolVersionTest:(enum RFBProtocolVersion)expectedVersion
				 serverToClientData:(NSData*)serverToClientData
	   injectableInputStream:(InjectableInputStream*)inputStream
{
	NSAssert(serverToClientData, @"serverToClientData is nil");
	NSAssert(inputStream, @"inputStream is nil");
	
	[inputStream appendData:serverToClientData];
	
	STAssertEquals([mRFB protocolVersion],
				   expectedVersion,
				   @"Didn't get protocol version (enum value %d)",
				   expectedVersion);
}

-(void)doProtocolVersionTestWithConnectAndResponseCheck:(enum RFBProtocolVersion)expectedVersion
								   serverProtocolString:(const char*)serverProtocolString
								  injectableInputStream:(InjectableInputStream**)inputStreamOut
								   readableOutputStream:(ReadableOutputStream**)outputStreamOut
{
	NSAssert(serverProtocolString, @"serverProtocolString is nil");
	NSAssert1(strlen(serverProtocolString) == PROTOCOL_VERSION_MSGLEN,
			  @"serverProtocolString has length %d", strlen(serverProtocolString));
	
	InjectableInputStream *is = nil;
	ReadableOutputStream *os = nil;
	
	[self doConnectInjectableInputStream:&is
					readableOutputStream:&os];
	
	NSData *serverToClientData = [NSData dataWithBytes:serverProtocolString
												length:strlen(serverProtocolString)];
	
	[self doProtocolVersionTest:expectedVersion
		   serverToClientData:serverToClientData
		  injectableInputStream:is];

	NSData *clientToServerMsg = [os readData];
	STAssertTrue([clientToServerMsg isEqualToData:serverToClientData],
				 @"Invalid response from client. Expected V %s",
				 serverProtocolString);
	
	if(inputStreamOut) *inputStreamOut = is;
	if(outputStreamOut) *outputStreamOut = os;
}

-(void)doProtocolVersionTest:(enum RFBProtocolVersion)expectedVersion
	   injectableInputStream:(InjectableInputStream**)inputStreamOut
				readableOutputStream:(ReadableOutputStream**)outputStreamOut
{
	const char *V3_3 = "RFB 003.003\n";
	const char *V3_7 = "RFB 003.007\n";
	const char *V3_8 = "RFB 003.008\n";
	const char *highestVersionSupportedByServerMessage = V3_3;
	
	switch(expectedVersion) {
		case RFBProtocolVersion_3_3:
			highestVersionSupportedByServerMessage = V3_3;
			break;
		case RFBProtocolVersion_3_7:
			highestVersionSupportedByServerMessage = V3_7;
			break;
		case RFBProtocolVersion_3_8:
			highestVersionSupportedByServerMessage = V3_8;
			break;
	}
	
	[self doProtocolVersionTestWithConnectAndResponseCheck:expectedVersion
									  serverProtocolString:highestVersionSupportedByServerMessage
									 injectableInputStream:inputStreamOut
									  readableOutputStream:outputStreamOut];
}

-(void)doProtocolVersionTest:(enum RFBProtocolVersion)expectedVersion
{
	[self doProtocolVersionTest:expectedVersion injectableInputStream:nil readableOutputStream:nil];
}

-(void)testProtocolVersion3_3
{
	[self doProtocolVersionTest:RFBProtocolVersion_3_3];
}

-(void)testProtocolVersion3_7
{
	[self doProtocolVersionTest:RFBProtocolVersion_3_7];
}

-(void)testProtocolVersion3_8
{
	[self doProtocolVersionTest:RFBProtocolVersion_3_8];
}

-(void)testProtocolVersion3_7In2Parts
{
	const enum RFBProtocolVersion DEFAULT_VERSION = RFBProtocolVersion_3_3;
	const char *V3_7 = "RFB 003.007\n";
	const unsigned int SPLIT = 5; // Split the message at byte number 5.
	
	NSData *dataPart1 = [NSData dataWithBytes:V3_7 length:PROTOCOL_VERSION_MSGLEN - SPLIT];
	NSData *dataPart2 = [NSData dataWithBytes:V3_7 + PROTOCOL_VERSION_MSGLEN - SPLIT
									   length:PROTOCOL_VERSION_MSGLEN - SPLIT];
	
	InjectableInputStream *is = [[[InjectableInputStream alloc] init] autorelease];
	ReadableOutputStream *os = [[[ReadableOutputStream alloc] init] autorelease];
	
	STAssertTrue([mRFB connectToInput:is output:os error:nil],
				 @"connectToInput failed");

	[self doProtocolVersionTest:DEFAULT_VERSION
			 serverToClientData:dataPart1
		  injectableInputStream:is];

	[self doProtocolVersionTest:RFBProtocolVersion_3_7
			 serverToClientData:dataPart2
		  injectableInputStream:is];
	
	NSData *expectedClientMessage = [NSData dataWithBytes:V3_7 length:strlen(V3_7)];
	STAssertTrue([[os readData] isEqualToData:expectedClientMessage],
				 @"Didn't get expected client string.");
}

-(void)testProtocolVersionOther
{
	const char *V_OTHER = "RFB 999.999\n";
	const char *V3_8 = "RFB 003.008\n";
	NSData *data = [NSData dataWithBytes:V_OTHER length:PROTOCOL_VERSION_MSGLEN];
	InjectableInputStream *is = [[[InjectableInputStream alloc] init] autorelease];
	ReadableOutputStream *os = [[[ReadableOutputStream alloc] init] autorelease];
	
	STAssertTrue([mRFB connectToInput:is output:os error:nil],
				 @"connectToInput failed");
	
	[self doProtocolVersionTest:RFBProtocolVersion_3_8
			 serverToClientData:data
		  injectableInputStream:is];
	
	NSData *expectedClientMessage = [NSData dataWithBytes:V3_8 length:strlen(V3_8)];
	STAssertTrue([[os readData] isEqualToData:expectedClientMessage],
				 @"Didn't get expected client string.");
}

-(void)doTestSecurityProtocol3_3:(enum RFBSecurityType)securityType
		   injectableInputStream:(InjectableInputStream**)inputStream
			readableOutputStream:(ReadableOutputStream**)outputStream
{
	InjectableInputStream *is = nil;
	ReadableOutputStream *os = nil;
	[self doProtocolVersionTest:RFBProtocolVersion_3_3
		  injectableInputStream:&is
		   readableOutputStream:&os];
	
	// Send the version 3.3 security message to the client.
	uint32_t type = htonl(securityType);
	[is appendData:&type numBytes:sizeof(type)];
	STAssertEquals([mRFB securityType], securityType, @"Didn't get expected security type from API");
	
	// Receive the response from the client.
	uint32_t responseType = 0;
	int bytesRead = [os readData:&responseType maxBytes:sizeof(responseType)];
	STAssertEquals(bytesRead, (int)sizeof(responseType),
				   @"Didn't get expected length of data security type response.");
	
	responseType = ntohl(responseType);
	STAssertEquals(responseType, (uint32_t)securityType, @"Didn't get expected security type response.");
	
	if(inputStream) *inputStream = is;
	if(outputStream) *outputStream = os;
}

-(void)doTestSecurityProtocol3_3:(enum RFBSecurityType)securityType
{
	[self doTestSecurityProtocol3_3:securityType
			  injectableInputStream:nil
			   readableOutputStream:nil];
}

-(void)testSecurityProtocol3_3_SecurityType_None
{
	[self doTestSecurityProtocol3_3:RFBSecurityType_None];
}

-(void)testSecurityProtocol3_3_SecurityType_VNCAuthentication
{
	[self doTestSecurityProtocol3_3:RFBSecurityType_VNCAuthentication];
}

-(void)doTestSecurityProtocol3_7_Onwards:(enum RFBSecurityType)expectedType
						 protocolVersion:(enum RFBProtocolVersion)protocolVersion
				   injectableInputStream:(InjectableInputStream**)inputStream
					readableOutputStream:(ReadableOutputStream**)outputStream
{
	InjectableInputStream *is = nil;
	ReadableOutputStream *os = nil;
	[self doProtocolVersionTest:protocolVersion
		  injectableInputStream:&is
		   readableOutputStream:&os];
	
	// Send the version 3.7 security message to the client.
	const uint8_t securityTypeList[] = {
		// Security types defined by standard.
		RFBSecurityType_None,
		RFBSecurityType_VNCAuthentication,
		
		// Other security types.
		RFBSecurityType_TLS,
		RFBSecurityType_Ultra,
		RFBSecurityType_Tight
	};
	
	const uint8_t numberOfTypes = sizeof(securityTypeList)/sizeof(securityTypeList[0]);
	[is appendData:&numberOfTypes numBytes:sizeof(numberOfTypes)];
	[is appendData:securityTypeList numBytes:sizeof(securityTypeList)];
	STAssertEquals([mRFB securityType], expectedType, @"Didn't get expected security type from API");
	
	// Receive the response from the client.
	uint8_t clientResponse = 0;
	int bytesRead = [os readData:&clientResponse maxBytes:sizeof(clientResponse)];
	STAssertEquals(bytesRead, (int)sizeof(clientResponse),
				   @"Didn't get expected length of data security type response.");
	
	STAssertEquals(clientResponse, (uint8_t)expectedType, @"Didn't get expected security type response.");
	
	if(inputStream) *inputStream = is;
	if(outputStream) *outputStream = os;
}

-(void)doTestSecurityProtocol3_7_Onwards:(enum RFBSecurityType)securityTypeToRequest
			  expectedClientSecurityType:(enum RFBSecurityType)expectedType
						 protocolVersion:(enum RFBProtocolVersion)protocolVersion
{
	SecurityTypeSelectorDelegate* delegate =
	[[SecurityTypeSelectorDelegate alloc] initWithSecurityType:securityTypeToRequest];
	[mRFB setDelegate:delegate];
	
	[self doTestSecurityProtocol3_7_Onwards:expectedType
							protocolVersion:protocolVersion
					  injectableInputStream:nil
					   readableOutputStream:nil];
	
	[mRFB setDelegate:nil]; // Disconnect our delegate before we release it.
	[delegate release];
}

-(void)testSecurityProtocol3_7_None
{
	[self doTestSecurityProtocol3_7_Onwards:RFBSecurityType_None
				 expectedClientSecurityType:RFBSecurityType_None
							protocolVersion:RFBProtocolVersion_3_7];
}

-(void)testSecurityProtocol3_7_VNCAuthentication
{
	[self doTestSecurityProtocol3_7_Onwards:RFBSecurityType_VNCAuthentication
				 expectedClientSecurityType:RFBSecurityType_VNCAuthentication
							protocolVersion:RFBProtocolVersion_3_7];
}

-(void)testSecurityProtocol3_8_VNCAuthentication
{
	[self doTestSecurityProtocol3_7_Onwards:RFBSecurityType_VNCAuthentication
				 expectedClientSecurityType:RFBSecurityType_VNCAuthentication
							protocolVersion:RFBProtocolVersion_3_8];
}

-(void)testSecurityProtocol3_8_NonExistentType
{
	[self doTestSecurityProtocol3_7_Onwards:255
				 expectedClientSecurityType:RFBSecurityType_Invalid
							protocolVersion:RFBProtocolVersion_3_8];
}

-(void)doTestSecurityResult:(enum RFBProtocolVersion) protocolVersion
	 expectedSecurityResult:(enum RFBSecurityResult) expectedSecurityResult
			 resultToClient:(enum RFBSecurityResult) resultToClient
			 reasonToClient:(NSString*)reasonToClient
			   securityType:(enum RFBSecurityType) securityType
	  injectableInputStream:(InjectableInputStream**)inputStream
	   readableOutputStream:(ReadableOutputStream**)outputStream
				   delegate:(SecurityTypeSelectorDelegate*)delegate
{	
	InjectableInputStream *is = nil;
	ReadableOutputStream *os = nil;
	
	if(protocolVersion == RFBProtocolVersion_3_3) {
		[self doTestSecurityProtocol3_3:securityType
				  injectableInputStream:&is
				   readableOutputStream:&os];
	} else {
		[self doTestSecurityProtocol3_7_Onwards:securityType
								protocolVersion:protocolVersion
						  injectableInputStream:&is
						   readableOutputStream:&os];
	}

	uint32_t securityResult = htonl(resultToClient);
	[is appendData:&securityResult numBytes:sizeof(securityResult)];
	
	if(securityResult == RFBSecurityResultFailed && protocolVersion >= RFBProtocolVersion_3_8) {
		uint32_t reasonLength = htonl([reasonToClient length]);
		[is appendData:&reasonLength numBytes:sizeof(reasonLength)];
		[is appendData:[reasonToClient UTF8String] numBytes:[reasonToClient length]];
	}
	
	STAssertEquals(expectedSecurityResult, [delegate securityResult],
				   @"Didn't get expected security result.");
	
	if(inputStream) *inputStream = is;
	if(outputStream) *outputStream = os;
}

-(void)doTestSecurityResult:(enum RFBProtocolVersion) protocolVersion
	 expectedSecurityResult:(enum RFBSecurityResult) expectedSecurityResult
			 reasonToClient:(NSString*)reasonToClient
			   securityType:(enum RFBSecurityType) securityType
{	
	SecurityTypeSelectorDelegate* delegate = [[SecurityTypeSelectorDelegate alloc] initWithSecurityType:securityType];
	[mRFB setDelegate:delegate];
	
	[self doTestSecurityResult:protocolVersion
		expectedSecurityResult:expectedSecurityResult
				resultToClient:expectedSecurityResult
				reasonToClient:reasonToClient
				  securityType:securityType
		 injectableInputStream:nil
		  readableOutputStream:nil
					  delegate:delegate];
	
	[mRFB setDelegate:nil];
	[delegate release];
}


-(void)testSecurityResult_V3_3
{
	[self doTestSecurityResult:RFBProtocolVersion_3_3
		expectedSecurityResult:RFBSecurityResultOK
				reasonToClient:nil
				  securityType:RFBSecurityType_None];
}

-(void)testSecurityResult_V3_7
{
	[self doTestSecurityResult:RFBProtocolVersion_3_7
		expectedSecurityResult:RFBSecurityResultOK
				reasonToClient:nil
				  securityType:RFBSecurityType_None];
}

-(void)testSecurityResult_V3_8
{
	[self doTestSecurityResult:RFBProtocolVersion_3_8
		expectedSecurityResult:RFBSecurityResultOK
				reasonToClient:nil
				  securityType:RFBSecurityType_None];
}

-(void)testSecurityResult_V3_8FailedWithReason
{
	[self doTestSecurityResult:RFBProtocolVersion_3_8
		expectedSecurityResult:RFBSecurityResultFailed
				reasonToClient:@"Some bogus reason string."
				  securityType:RFBSecurityType_None];
}

-(void)XtestVNCAuthentication
{
	// TODO: Test VNC Authentication.
}

-(void)doTestClientInit:(enum RFBProtocolVersion) protocolVersion
		   securityType:(enum RFBSecurityType) securityType
			   password:(NSString*)password
  injectableInputStream:(InjectableInputStream**)inputStream
   readableOutputStream:(ReadableOutputStream**)outputStream
			   delegate:(SecurityTypeSelectorDelegate*)delegate
{
	InjectableInputStream *is = nil;
	ReadableOutputStream *os = nil;
	
	[self doTestSecurityResult:protocolVersion
		expectedSecurityResult:RFBSecurityResultOK
				resultToClient:RFBSecurityResultOK
				reasonToClient:nil
				  securityType:securityType
		 injectableInputStream:&is
		  readableOutputStream:&os
					  delegate:delegate];
	
	uint8_t sharedFlags = 0;
	int bytesRead = [os readData:&sharedFlags maxBytes:sizeof(sharedFlags)];
	STAssertEquals(bytesRead, (int)sizeof(sharedFlags), @"Didn't get 1 bytes for shared flags from ClientInit.");
	sharedFlags = ntohl(sharedFlags);
	STAssertEquals(sharedFlags, (uint8_t)RFBClientInit_ExclusiveDesktopAccess, @"Didn't get exclusive desktop access.");
	
	if(inputStream) *inputStream = is;
	if(outputStream) *outputStream = os;
}

-(void)doTestClientInit:(enum RFBProtocolVersion) protocolVersion
{	
	enum RFBSecurityType securityType = RFBSecurityType_None;
	SecurityTypeSelectorDelegate* delegate = [[SecurityTypeSelectorDelegate alloc] initWithSecurityType:securityType];
	[mRFB setDelegate:delegate];
	
	[self doTestClientInit:protocolVersion
			  securityType:securityType
				  password:nil
	 injectableInputStream:nil
	  readableOutputStream:nil
				  delegate:delegate];
	
	[mRFB setDelegate:nil];
	[delegate release];
}

-(void)testClientInit_V3_3
{
	[self doTestClientInit:RFBProtocolVersion_3_3];		
}

-(void)testClientInit_V3_8
{
	[self doTestClientInit:RFBProtocolVersion_3_8];	
}

static const uint16_t FRAME_BUFFER_WIDTH = 640;
static const uint16_t FRAME_BUFFER_HEIGHT = 480;

-(void)doTestServerInit:(enum RFBProtocolVersion) protocolVersion
		   securityType:(enum RFBSecurityType) securityType
			   password:(NSString*)password
  injectableInputStream:(InjectableInputStream**)inputStream
   readableOutputStream:(ReadableOutputStream**)outputStream
			   delegate:(SecurityTypeSelectorDelegate*)delegate
{
	InjectableInputStream *is = nil;
	ReadableOutputStream *os = nil;
	NSString *desktopName = @"RFB Desktop Name for Unit Testing";
	
	[self doTestClientInit:protocolVersion
			  securityType:securityType
				  password:password
	 injectableInputStream:&is
	  readableOutputStream:&os
				  delegate:delegate];
	
	uint16_t frameBufferWidth = htons(FRAME_BUFFER_WIDTH);
	uint16_t frameBufferHeight = htons(FRAME_BUFFER_HEIGHT);
	
	// Pixel Format Structure
	uint8_t bitsPerPixel = 32;
	uint8_t depth = 24;
	uint8_t bigEndianFlag = 0; // bytes are little endian.
	uint8_t trueColorFlag = 1; // Use true color
	uint16_t redMax = htons(0xff);
	uint16_t greenMax = htons(0xff);
	uint16_t blueMax = htons(0xff);
	uint8_t redShift = 24;
	uint8_t greenShift = 16;
	uint8_t blueShift = 8;
	uint8_t padding[3];
	
	uint32_t nameLength = htonl([desktopName length]);
	uint8_t *name = (uint8_t*)[desktopName UTF8String];

	[is appendData:&frameBufferWidth numBytes:sizeof(frameBufferWidth)];
	[is appendData:&frameBufferHeight numBytes:sizeof(frameBufferHeight)];
	
	[is appendData:&bitsPerPixel numBytes:sizeof(bitsPerPixel)];
	[is appendData:&depth numBytes:sizeof(depth)];
	[is appendData:&bigEndianFlag numBytes:sizeof(bigEndianFlag)];
	[is appendData:&trueColorFlag numBytes:sizeof(trueColorFlag)];
	[is appendData:&redMax numBytes:sizeof(redMax)];
	[is appendData:&greenMax numBytes:sizeof(greenMax)];
	[is appendData:&blueMax numBytes:sizeof(blueMax)];
	[is appendData:&redShift numBytes:sizeof(redShift)];
	[is appendData:&greenShift numBytes:sizeof(greenShift)];
	[is appendData:&blueShift numBytes:sizeof(blueShift)];
	[is appendData:&padding numBytes:sizeof(padding)];
	
	[is appendData:&nameLength numBytes:sizeof(nameLength)];
	[is appendData:name numBytes:[desktopName length]];
	
	STAssertTrue([desktopName isEqual:[delegate desktopName]], @"Didn't get expected desktop name.");
	
	if(inputStream) *inputStream = is;
	if(outputStream) *outputStream = os;
}

-(void)doTestServerInit:(enum RFBProtocolVersion) protocolVersion
{	
	enum RFBSecurityType securityType = RFBSecurityType_None;
	SecurityTypeSelectorDelegate* delegate = [[SecurityTypeSelectorDelegate alloc] initWithSecurityType:securityType];
	[mRFB setDelegate:delegate];
	
	[self doTestServerInit:protocolVersion
			  securityType:RFBSecurityType_None
				  password:nil
	 injectableInputStream:nil
	  readableOutputStream:nil
				  delegate:delegate];
	
	[mRFB setDelegate:nil];
	[delegate release];
}

-(void)testServerInit_V3_3
{
	[self doTestServerInit:RFBProtocolVersion_3_3];
}

-(void)testServerInit_V3_7
{
	[self doTestServerInit:RFBProtocolVersion_3_7];
}

-(void)testServerInit_V3_8
{
	[self doTestServerInit:RFBProtocolVersion_3_8];
}

-(void)doTestSetFormat:(enum RFBProtocolVersion)protocolVersion
 injectableInputStream:(InjectableInputStream**)inputStream
  readableOutputStream:(ReadableOutputStream**)outputStream
		  securityType:(enum RFBSecurityType)securityType
			  password:(NSString*)password
			  delegate:(id)delegate
{	
	InjectableInputStream *is = nil;
	ReadableOutputStream *os = nil;
	
	[self doTestServerInit:protocolVersion
			  securityType:securityType
				  password:password
	 injectableInputStream:&is
	  readableOutputStream:&os
				  delegate:delegate];
	
	uint8_t messageType, padding1[3];
	uint8_t bitsPerPixel, depth, bigEndianFlag, trueColorFlag;
	uint16_t redMax, greenMax, blueMax;
	uint8_t redShift, greenShift, blueShift;
	uint8_t pixelFormatPadding[3];
	int bytesRead;
	
	bytesRead = [os readData:&messageType maxBytes:sizeof(messageType)];
	STAssertEquals(bytesRead, (int)sizeof(messageType), @"Didn't read entire message type.");
	STAssertEquals(messageType, (uint8_t)0, @"Didn't get expected (SetPixelFormat) message.");
	bytesRead = [os readData:padding1 maxBytes:sizeof(padding1)];
	STAssertEquals(bytesRead, (int)sizeof(padding1), @"Didn't get expected padding.");
	
	bytesRead = [os readData:&bitsPerPixel maxBytes:sizeof(bitsPerPixel)];
	STAssertEquals(bytesRead, (int)sizeof(bitsPerPixel), @"Didn't get enough bytes for bitsPerPixel");
	STAssertEquals(bitsPerPixel, (uint8_t)32, @"Didn't get expected bitsPerPixel");
	
	bytesRead = [os readData:&depth maxBytes:sizeof(depth)];
	STAssertEquals(bytesRead, (int)sizeof(depth), @"Didn't get enough bytes for depth");
	STAssertEquals(depth, (uint8_t)24, @"Didn't get expected depth");
	
	bytesRead = [os readData:&bigEndianFlag maxBytes:sizeof(bigEndianFlag)];
	STAssertEquals(bytesRead, (int)sizeof(bigEndianFlag), @"Didn't get enough bytes for bigEndianFlag");
	STAssertFalse(bigEndianFlag, @"Didn't get expected bigEndianFlag");
	
	bytesRead = [os readData:&trueColorFlag maxBytes:sizeof(trueColorFlag)];
	STAssertEquals(bytesRead, (int)sizeof(trueColorFlag), @"Didn't get enough bytes for trueColorFlag");
	STAssertTrue(trueColorFlag, @"Didn't get expected trueColorFlag");
	
	bytesRead = [os readData:&redMax maxBytes:sizeof(redMax)];
	STAssertEquals(bytesRead, (int)sizeof(redMax), @"Didn't get enough bytes for redMax");
	redMax = ntohs(redMax);
	STAssertEquals(redMax, (uint16_t)0xff, @"Didn't get expected redMax");
	
	bytesRead = [os readData:&greenMax maxBytes:sizeof(greenMax)];
	STAssertEquals(bytesRead, (int)sizeof(greenMax), @"Didn't get enough bytes for greenMax");
	greenMax = ntohs(greenMax);
	STAssertEquals(greenMax, (uint16_t)0xff, @"Didn't get expected greenMax");
	
	bytesRead = [os readData:&blueMax maxBytes:sizeof(blueMax)];
	STAssertEquals(bytesRead, (int)sizeof(blueMax), @"Didn't get enough bytes for blueMax");
	blueMax = ntohs(blueMax);
	STAssertEquals(blueMax, (uint16_t)0xff, @"Didn't get expected blueMax");
	
	bytesRead = [os readData:&redShift maxBytes:sizeof(redShift)];
	STAssertEquals(bytesRead, (int)sizeof(redShift), @"Didn't get enough bytes for redShift");
	STAssertEquals(redShift, (uint8_t)0, @"Didn't get expected redShift");
	
	bytesRead = [os readData:&greenShift maxBytes:sizeof(greenShift)];
	STAssertEquals(bytesRead, (int)sizeof(greenShift), @"Didn't get enough bytes for greenShift");
	STAssertEquals(greenShift, (uint8_t)8, @"Didn't get expected greenShift");
	
	bytesRead = [os readData:&blueShift maxBytes:sizeof(blueShift)];
	STAssertEquals(bytesRead, (int)sizeof(blueShift), @"Didn't get enough bytes for blueShift");
	STAssertEquals(blueShift, (uint8_t)16, @"Didn't get expected blueShift");
	
	bytesRead = [os readData:pixelFormatPadding maxBytes:sizeof(pixelFormatPadding)];
	STAssertEquals(bytesRead, (int)sizeof(pixelFormatPadding), @"Didn't get enough bytes for pixelFormatPadding");
	
	if(inputStream) *inputStream = is;
	if(outputStream) *outputStream = os;
}

-(void)doTestSetFormat:(enum RFBProtocolVersion)protocolVersion
{
	enum RFBSecurityType securityType = RFBSecurityType_None;
	SecurityTypeSelectorDelegate* delegate = [[SecurityTypeSelectorDelegate alloc] initWithSecurityType:securityType];
	[mRFB setDelegate:delegate];
	
	[self doTestSetFormat:protocolVersion
	injectableInputStream:nil
	 readableOutputStream:nil
			 securityType:RFBSecurityType_None
				 password:nil
				 delegate:delegate];
	
	[mRFB setDelegate:nil];
	[delegate release];
}

-(void)testSetFormat_V3_3
{
	[self doTestSetFormat:RFBProtocolVersion_3_3];
}

-(void)testSetFormat_V3_7
{
	[self doTestSetFormat:RFBProtocolVersion_3_7];
}

-(void)testSetFormat_V3_8
{
	[self doTestSetFormat:RFBProtocolVersion_3_8];
}

-(void)doRequestFrameBufferUpdate:(enum RFBProtocolVersion)protocolVersion
{
	enum RFBSecurityType securityType = RFBSecurityType_None;
	SecurityTypeSelectorDelegate* delegate = [[SecurityTypeSelectorDelegate alloc] initWithSecurityType:securityType];
	[mRFB setDelegate:delegate];
	
	InjectableInputStream *is = nil;
	ReadableOutputStream *os = nil;
	
	uint8_t messageType, isIncremental;
	uint16_t xPosition, yPosition, width, height;
	int bytesRead;
	
	[self doTestSetFormat:protocolVersion
	injectableInputStream:&is
	 readableOutputStream:&os
			 securityType:RFBSecurityType_None
				 password:nil
				 delegate:delegate];
	
	// Get the request.
	bytesRead = [os readData:&messageType maxBytes:sizeof(messageType)];
	STAssertEquals(bytesRead, (int)sizeof(messageType), @"Didn't get enough bytes for messageType");
	STAssertEquals(messageType, (uint8_t)3, @"Didn't get expected messageType");
	
	bytesRead = [os readData:&isIncremental maxBytes:sizeof(isIncremental)];
	STAssertEquals(bytesRead, (int)sizeof(isIncremental), @"Didn't get enough bytes for isIncremental");
	STAssertFalse(isIncremental, @"Didn't get expected value for isIncremental");
		
	bytesRead = [os readData:&xPosition maxBytes:sizeof(xPosition)];
	STAssertEquals(bytesRead, (int)sizeof(xPosition), @"Didn't get enough bytes for xPosition");
	xPosition = ntohs(xPosition);
	STAssertEquals(xPosition, (uint16_t)0, @"Didn't get expected value for xPosition");
	
	[os readData:&yPosition maxBytes:sizeof(yPosition)];
	STAssertEquals(bytesRead, (int)sizeof(yPosition), @"Didn't get enough bytes for yPosition");
	yPosition = ntohs(yPosition);
	STAssertEquals(yPosition, (uint16_t)0, @"Didn't get expected value for yPosition");
	
	[os readData:&width maxBytes:sizeof(width)];
	STAssertEquals(bytesRead, (int)sizeof(width), @"Didn't get enough bytes for width");
	width = ntohs(width);
	STAssertEquals(width, (uint16_t)FRAME_BUFFER_WIDTH, @"Didn't get expected value for width");
	
	[os readData:&height maxBytes:sizeof(height)];
	STAssertEquals(bytesRead, (int)sizeof(height), @"Didn't get enough bytes for height");
	height = ntohs(height);
	STAssertEquals(height, (uint16_t)FRAME_BUFFER_HEIGHT, @"Didn't get expected value for height");
	
	// Send the response.
	{
		uint8_t messageType;//, isIncremental;
		uint16_t xPosition, yPosition, width, height;
		uint8_t padding = 0;
		uint16_t numberOfRectangles = htons(1);
		
		const uint8_t SERVER_TO_CLIENT_FramebufferUpdate = 0;
		messageType = SERVER_TO_CLIENT_FramebufferUpdate;
		
		// Send message header.
		[is appendData:&messageType numBytes:sizeof(messageType)];
		[is appendData:&padding numBytes:sizeof(padding)];
		[is appendData:&numberOfRectangles numBytes:sizeof(numberOfRectangles)];
		
		// Send first rectangle header.
		xPosition = htons(0);
		[is appendData:&xPosition numBytes:sizeof(xPosition)];
		yPosition = htons(0);
		[is appendData:&yPosition numBytes:sizeof(yPosition)];
		width = htons(FRAME_BUFFER_WIDTH);
		[is appendData:&width numBytes:sizeof(width)];
		height = htons(FRAME_BUFFER_HEIGHT);
		[is appendData:&height numBytes:sizeof(height)];
		const int32_t RAW_ENCODING = 0;
		int32_t encodingType = ntohl(RAW_ENCODING);
		[is appendData:&encodingType numBytes:sizeof(encodingType)];
		
		// Send first rectangle pixel data.
		const unsigned pixelCount = FRAME_BUFFER_WIDTH * FRAME_BUFFER_HEIGHT;
		const unsigned pixelDataByteCount = pixelCount * sizeof(uint32_t);
		uint32_t *pixelData = (uint32_t*)malloc(pixelDataByteCount);
		int i;
		for(i = 0; i < pixelCount; ++i) {
			// Set the pixel data in big Endian format.
			uint8_t *r = (uint8_t*)(pixelData + i);
			uint8_t *g = r + 1;
			uint8_t *b = g + 1;
			uint8_t *a = b + 1;
			
			// Set the RGB data using uint8_t pointers because we want to set it
			// in big endian format, regardless of the byte ordering of the machine
			// running this code.
			*r = 0xff;
			*g = *b = *a = 0;
		}
		[is appendData:pixelData numBytes:pixelDataByteCount];
		
		id <RemoteFrameBufferUpdate> rfbUpdate = [delegate remoteFrameBufferUpdate];
		STAssertNotNil(rfbUpdate, @"rfbUpdate is nil");
		STAssertEquals([rfbUpdate height], FRAME_BUFFER_HEIGHT,
					   @"Didn't get expected height in RFB Event");
		STAssertEquals([rfbUpdate width], FRAME_BUFFER_WIDTH,
					   @"Didn't get expected width in RFB Event.");
		
		STAssertTrue(memcmp([[rfbUpdate pixleData] bytes], pixelData, pixelDataByteCount) == 0,
					 @"Pixel data in RFB Update Message doesn't match pixel data sent.");
		
		free(pixelData);
		pixelData = 0;
	}
	
	[mRFB setDelegate:nil];
	[delegate release];
}

-(void)testRequestFrameBufferUpdate_V3_3
{
	[self doRequestFrameBufferUpdate:RFBProtocolVersion_3_3];
}

-(void)testRequestFrameBufferUpdate_V3_7
{
	[self doRequestFrameBufferUpdate:RFBProtocolVersion_3_7];
}

-(void)testRequestFrameBufferUpdate_V3_8
{
	[self doRequestFrameBufferUpdate:RFBProtocolVersion_3_8];
}

-(void)doTestKeyEvent:(enum RFBKeyEventKeyState)keystate
			   keysym:(uint32_t)keysym
{
	InjectableInputStream *is = nil;
	ReadableOutputStream *os = nil;
	
	[self doConnectInjectableInputStream:&is
					readableOutputStream:&os];
	
	[mRFB sendKeyEvent:keystate
				keysym:keysym];
	
	uint8_t messageType = 0;
	uint8_t downFlag = 0;
	uint8_t padding[2];
	uint32_t keysymReceived = 0;
	
	[os readData:&messageType maxBytes:sizeof(messageType)];
	[os readData:&downFlag maxBytes:sizeof(downFlag)];
	[os readData:padding maxBytes:sizeof(padding)];
	[os readData:&keysymReceived maxBytes:sizeof(keysymReceived)];
	keysymReceived = ntohl(keysymReceived);
	
	STAssertEquals((int)messageType, 4, @"Unexpected message type.");
	STAssertEquals((int)downFlag, (int)keystate, @"Unexpected downFlag");
	STAssertEquals(keysymReceived, keysym, @"Didn't get expected keysym");
}

-(void)testKeyEventPressedA
{
	[self doTestKeyEvent:RFBKeyEventKeyStatePressed
				  keysym:'a'];
}

-(void)testKeyEventReleasedA
{
	[self doTestKeyEvent:RFBKeyEventKeyStateReleased
				  keysym:'a'];
}


-(void)doTestPointerEvent:(uint8_t)downButtonMask
				xPosition:(uint16_t)xPosition
				yPosition:(uint16_t)yPosition
{
	InjectableInputStream *is = nil;
	ReadableOutputStream *os = nil;
	
	[self doConnectInjectableInputStream:&is
					readableOutputStream:&os];
	
	[mRFB sendPointerEvent:downButtonMask
				 xPosition:xPosition
				 yPosition:yPosition];
	
	uint8_t messageType = 0;
	uint8_t downButtonMaskReceived = 0;
	uint16_t xPosRecv = 0, yPosRecv = 0;
	
	[os readData:&messageType maxBytes:sizeof(messageType)];
	[os readData:&downButtonMaskReceived maxBytes:sizeof(downButtonMaskReceived)];
	[os readData:&xPosRecv maxBytes:sizeof(xPosRecv)];
	xPosRecv = ntohs(xPosRecv);
	[os readData:&yPosRecv maxBytes:sizeof(yPosRecv)];
	yPosRecv = ntohs(yPosRecv);
	
	STAssertEquals((int)messageType, 5, @"Unexpected message type.");
	STAssertEquals((int)downButtonMask, (int)downButtonMaskReceived, @"Unexpected downFlag");
	STAssertEquals(xPosRecv, xPosition, @"Didn't get expected xPosition");
	STAssertEquals(yPosRecv, yPosition, @"Didn't get expected yPosition");
}

-(void)testPointerEvent000
{
	[self doTestPointerEvent:0
				   xPosition:0
				   yPosition:0];
}

-(void)testPointerEventLeftButtonDownAt300x200
{
	[self doTestPointerEvent:RFBPointerButton1Down
				   xPosition:300
				   yPosition:200];
}

-(void)testPointerEventMiddleButtonDownAt800x600
{
	[self doTestPointerEvent:RFBPointerButton2Down
				   xPosition:800
				   yPosition:600];
}

-(void)testPointerEventRightButtonDownAt1002x230
{
	[self doTestPointerEvent:RFBPointerButton3Down
				   xPosition:1002
				   yPosition:230];
}

-(void)testPointerEventMaxOut
{
	[self doTestPointerEvent:0xff
				   xPosition:0xffff
				   yPosition:0xffff];
}

@end
