//
//  RemoteFrameBufferProtocol.h
//  VNCClient
//
//  Created by Douglas Richardson on 3/20/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>

enum RFBProtocolState
{
	RFBPNoConnection,
	RFBPConnected,
	RFBPGotProtocolVersion,
	RFBPGotSecurityType,
	RFBPGotOKSecurityResult,
	RFBPAuthenticated,
	RFBPSentClientInit,
	RFBPNormalOperation
};

enum RFBProtocolVersion
{
	RFBProtocolVersion_3_3,
	RFBProtocolVersion_3_7,
	RFBProtocolVersion_3_8
};

enum RFBSecurityType
{
	// Security types defined by RFB 3.8
	RFBSecurityType_Invalid = 0,
	RFBSecurityType_None = 1,
	RFBSecurityType_VNCAuthentication = 2,
	
	// Other security types.
	RFBSecurityType_RA2 = 5,
	RFBSecurityType_RA2ne = 6,
	RFBSecurityType_Tight = 16,
	RFBSecurityType_Ultra = 17,
	RFBSecurityType_TLS = 18,
	RFBSecurityType_VeNCrypt = 19
};

enum RFBDelegateEventType
{
	RFBEventType_HandshakeError,
	RFBEventType_SelectSecurityType,
	RFBEventType_SecurityResult,
	RFBEventType_ServerInit,
	RFBEventType_FrameBufferUpdate,
	RFBEventType_RingBell,
	RFBEventType_ServerCutText,
	RFBEventType_ConnectionClosed
};

enum RFBClientInitOptions
{
	RFBClientInit_ExclusiveDesktopAccess = 0,
	RFBClientInit_SharedDesktopAccess = 1
};

extern NSString * const RFBResultMessageKey;
extern NSString * const RFBSecurityListKey;
extern NSString * const RFBSecurityResultKey;
extern NSString * const RFBDesktopNameKey;
extern NSString * const RFBFrameBufferUpdateMessageKey;
extern NSString * const RFBServerCutTextKey;

enum RFBSecurityResult
{
	RFBSecurityResultInvalid = -1,
	RFBSecurityResultOK = 0,
	RFBSecurityResultFailed = 1
};

@protocol RemoteFrameBufferUpdate
-(uint16_t) xPosition;
-(uint16_t) yPosition;
-(uint16_t) width;
-(uint16_t) height;
-(NSData*) pixleData;
@end

@interface RemoteFrameBufferProtocol : NSObject {
	enum RFBProtocolVersion mProtocolVersion;
	enum RFBSecurityType mSecurityType;
	
	NSInputStream *mInputStream;
	NSOutputStream *mOutputStream;
	
	NSMutableData *mOutputStreamStagingArea;
	
	enum RFBProtocolState mProtocolState;
	NSMutableDictionary *mStageState;
	NSMutableData *mStageData;
	
	id mDelegate;
	NSInvocation *mInvocation;
	
	enum RFBClientInitOptions mClientInitOptions;
	
	uint16_t mFrameBufferWidth;
	uint16_t mFrameBufferHeight;
	
	NSString *mDesktopName;
	NSBitmapImageRep *mLocalFrameBuffer;
}

-(id)initWithClientInit:(enum RFBClientInitOptions)clientInitOptions;

-(uint16_t)frameBufferWidth;
-(uint16_t)frameBufferHeight;

-(enum RFBClientInitOptions)clientInitOptions;

-(NSString*)desktopName;
-(NSBitmapImageRep*)localFrameBuffer;

// A delegate must implement the event handler method with the following signature:
// -(id) rfbEvent:(enum RFBDelegateEventType)eventType eventData:(NSDictionary*)eventData
// Keys to the eventData dictionary are defined through extern NSString * above.
-(id)delegate;
-(void)setDelegate:(id)delegate;

-(enum RFBProtocolVersion)protocolVersion;
-(enum RFBSecurityType)securityType;

-(BOOL)connectToHost:(NSString*)hostname
				port:(uint16_t)port
			   error:(NSError**)error;

-(BOOL)connectToInput:(NSInputStream*)inputStream
			   output:(NSOutputStream*)outputStream
				error:(NSError**)error;

-(void)disconnect;

enum RFBKeyEventKeyState
{
	RFBKeyEventKeyStateReleased = 0,
	RFBKeyEventKeyStatePressed = 1
};

// For full details of the keysum argument, see The Xlib Reference Manual, published
// by O’Reilly & Associates, or see the header file <X11/keysymdef.h> from any
// X Window System installation.
-(void)sendKeyEvent:(enum RFBKeyEventKeyState)state
			 keysym:(uint32_t)keysym;

// The RFBPointerButtonXDown constants are used to create bitmasks
// that indicate which buttons are pressed down. Typical mappings of
// a conventional mouse appear in the comments below.
extern const uint8_t RFBPointerButton1Down; // Left Button
extern const uint8_t RFBPointerButton2Down; // Middle Button
extern const uint8_t RFBPointerButton3Down; // Right Button
extern const uint8_t RFBPointerButton4Down; // Press+Release = Wheel Up One Step
extern const uint8_t RFBPointerButton5Down; // Press+Release = Wheel Down One Step
extern const uint8_t RFBPointerButton6Down;
extern const uint8_t RFBPointerButton7Down;
extern const uint8_t RFBPointerButton8Down;

-(void)sendPointerEvent:(uint8_t)downButtonMask
			  xPosition:(uint16_t)xPosition
			  yPosition:(uint16_t)yPosition;

-(void)sendPaste:(NSString*)textToPaste;
@end
