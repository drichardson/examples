//
//  VNCView.m
//  VNCClient
//
//  Created by Douglas Richardson on 3/20/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.

#import "VNCView.h"
#import "RemoteFrameBufferProtocol.h"

#define XK_MISCELLANY
#import <X11/keysymdef.h>

@implementation VNCView

- (id)initWithFrame:(NSRect)frameRect
{
	if ((self = [super initWithFrame:frameRect]) != nil) {
		mBellSound = [[NSSound soundNamed:@"Pop"] retain];
	}
	return self;
}

-(void)setLastPasteboardData:(NSString*)newPBData
{
	if(mLastPasteboardData != newPBData) {
		[mLastPasteboardData release];
		mLastPasteboardData = [newPBData copy];
	}
}

-(void) dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	
	[self setRemoteFrameBufferProtocol:nil];
	
	[mBellSound release];
	mBellSound = nil;
	
	[self setLastPasteboardData:nil];
	
	[super dealloc];
}

-(void)setRemoteFrameBufferProtocol:(RemoteFrameBufferProtocol*)newRFB
{
	[mRFB setDelegate:nil];
	[mRFB disconnect];
	[mRFB release];
	mRFB = [newRFB retain];
	[mRFB setDelegate:self];
}

-(void)becameKeyWindow:(NSNotification *)notification
{
	if([[self window] isKeyWindow]) {
		NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
		NSArray *pasteTypes = [NSArray arrayWithObjects:NSStringPboardType, nil];
		NSString *bestType = [pasteboard availableTypeFromArray:pasteTypes];
		
		if(bestType != nil) {
			NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
			NSString *pbData = [pasteboard stringForType:NSStringPboardType];
			if(pbData && ![pbData isEqual:mLastPasteboardData]) {
				[self setLastPasteboardData:pbData];
				[mRFB sendPaste:mLastPasteboardData];
			}
		}
	}
}

- (void)viewDidMoveToWindow
{	
	[[self window] setAcceptsMouseMovedEvents:YES];
	
	// Register to be notified every time the parent window becomes the
	// key window. This is used to update the Client Cut Buffer.
	[[NSNotificationCenter defaultCenter] addObserver:self
											 selector:@selector(becameKeyWindow:)
												 name:@"NSWindowDidBecomeKeyNotification" object:nil];
}

- (void)drawRect:(NSRect)rect
{
	if([mRFB localFrameBuffer]) {		
		NSImage *image = [[NSImage alloc] init];
		[image addRepresentation:[mRFB localFrameBuffer]];
		[image drawInRect:rect // TODO: what if the rect is only part of the window? This is wrong.
				 fromRect:NSZeroRect
				operation:NSCompositeSourceOver
				 fraction:1.0];
		
		[image release];	
	} else {
		NSLog(@"drawRect: no update received. Just erase everything.");
		NSEraseRect(rect);
	}
}

-(id) rfbEvent:(enum RFBDelegateEventType)eventType
	 eventData:(NSDictionary*)eventData
{
	if(eventType == RFBEventType_FrameBufferUpdate) {
		[self setNeedsDisplay:YES];		
	} else if(eventType == RFBEventType_ConnectionClosed) {
		NSAlert *alert = [[NSAlert alloc] init];
		[alert addButtonWithTitle:@"OK"];
		[alert setMessageText:@"Connection closed."];
		[alert setInformativeText:[eventData objectForKey:RFBResultMessageKey]];
		[alert runModal];
		[alert release];
	} else if(eventType == RFBEventType_RingBell) {
		NSLog(@"Ring Bell received");
		[mBellSound play];
	} else if(eventType == RFBEventType_ServerCutText) {
		NSString *text = [eventData objectForKey:RFBServerCutTextKey];
		NSLog([NSString stringWithFormat:@"Got server cut text '%@' from server.", text]);

		NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
		NSArray *types = [NSArray arrayWithObject:NSStringPboardType];
		[pasteboard declareTypes:types owner:self];
		[pasteboard setString:text forType:NSStringPboardType];
	}
	
	return nil;
}

- (BOOL)acceptsFirstResponder
{
	return YES;
}

- (void) sendPointerEvent:(NSEvent*)event
{
	NSPoint eventLocation = [event locationInWindow];
	
	// Only process events over this view.
	if([self hitTest:eventLocation] == self) {
		NSPoint center = [self convertPoint:eventLocation fromView:nil];
		
		// Map client coordinates to server coordinates.
		NSRect b = [self bounds];
		
		// Convert to server coordinates.
		uint16_t xPosition = (center.x / b.size.width) * ((double)[mRFB frameBufferWidth]);
		uint16_t yPosition = ((double)[mRFB frameBufferHeight]) * (1 - center.y / b.size.height);
		
		[mRFB sendPointerEvent:mMouseButtonState
					 xPosition:xPosition
					 yPosition:yPosition];
	}
}

-(void)addMouseState:(uint8_t)bit
{
	mMouseButtonState |= bit;
}

-(void)removeMouseState:(uint8_t)bit
{
	uint8_t removeMask = ~bit;
	mMouseButtonState &= removeMask;
}

- (void)mouseMoved:(NSEvent *)event
{
	[self sendPointerEvent:event];
}


// Left Mouse Button Events
- (void)mouseDown:(NSEvent *)event
{
	[self addMouseState:RFBPointerButton1Down];
	[self sendPointerEvent:event];
}

- (void)mouseUp:(NSEvent *)event
{
	[self removeMouseState:RFBPointerButton1Down];
	[self sendPointerEvent:event];
}

- (void)mouseDragged:(NSEvent *)event
{
	[self sendPointerEvent:event];
}

// Right Mouse Button Events
- (void)rightMouseDown:(NSEvent *)event
{
	[self addMouseState:RFBPointerButton3Down];
	[self sendPointerEvent:event];	
}

- (void)rightMouseUp:(NSEvent *)event
{
	[self removeMouseState:RFBPointerButton3Down];
	[self sendPointerEvent:event];
}

- (void)rightMouseDragged:(NSEvent *)event
{
	[self sendPointerEvent:event];
}

// Scroll Wheel

- (void)scrollWheel:(NSEvent *)event
{
	NSLog([NSString stringWithFormat:@"scrollWheel: (dX,dY,dZ)=(%f,%f,%f)",
		[event deltaX], [event deltaY], [event deltaZ]]);
	
	uint8_t mask = 0;
	
	if([event deltaY] > 0) {
		// scroll up
		mask = RFBPointerButton4Down;
		
	} else if([event deltaY] < 0) {
		// scroll down
		mask = RFBPointerButton5Down;
	}
	
	[self addMouseState:mask];
	[self sendPointerEvent:event];
	[self removeMouseState:mask];
	[self sendPointerEvent:event];
}

#define ASCII_ESC 27
#define ASCII_DEL 127

static BOOL translateUnicharToX11Keysym(unichar uc, uint32_t *pKeysym)
{
	uint32_t ks = 0;
	const uint32_t NO_MAPPING = 0;
	
	switch(uc) {
		case NSUpArrowFunctionKey: ks = XK_Up; break;
		case NSDownArrowFunctionKey: ks = XK_Down; break;
		case NSLeftArrowFunctionKey: ks = XK_Left; break;
		case NSRightArrowFunctionKey: ks = XK_Right; break;
		case NSF1FunctionKey: ks = XK_F1; break;
		case NSF2FunctionKey: ks = XK_F2; break;
		case NSF3FunctionKey: ks = XK_F3; break;
		case NSF4FunctionKey: ks = XK_F4; break;
		case NSF5FunctionKey: ks = XK_F5; break;
		case NSF6FunctionKey: ks = XK_F6; break;
		case NSF7FunctionKey: ks = XK_F7; break;
		case NSF8FunctionKey: ks = XK_F8; break;
		case NSF9FunctionKey: ks = XK_F9; break;
		case NSF10FunctionKey: ks = XK_F10; break;
		case NSF11FunctionKey: ks = XK_F11; break;
		case NSF12FunctionKey: ks = XK_F12; break;
		case NSF13FunctionKey: ks = XK_F13; break;
		case NSF14FunctionKey: ks = XK_F14; break;
		case NSF15FunctionKey: ks = XK_F15; break;
		case NSF16FunctionKey: ks = XK_F16; break;
		case NSF17FunctionKey: ks = XK_F17; break;
		case NSF18FunctionKey: ks = XK_F18; break;
		case NSF19FunctionKey: ks = XK_F19; break;
		case NSF20FunctionKey: ks = XK_F20; break;
		case NSF21FunctionKey: ks = XK_F21; break;
		case NSF22FunctionKey: ks = XK_F22; break;
		case NSF23FunctionKey: ks = XK_F23; break;
		case NSF24FunctionKey: ks = XK_F24; break;
		case NSF25FunctionKey: ks = XK_F25; break;
		case NSF26FunctionKey: ks = XK_F26; break;
		case NSF27FunctionKey: ks = XK_F27; break;
		case NSF28FunctionKey: ks = XK_F28; break;
		case NSF29FunctionKey: ks = XK_F29; break;
		case NSF30FunctionKey: ks = XK_F30; break;
		case NSF31FunctionKey: ks = XK_F31; break;
		case NSF32FunctionKey: ks = XK_F32; break;
		case NSF33FunctionKey: ks = XK_F33; break;
		case NSF34FunctionKey: ks = XK_F34; break;
		case NSF35FunctionKey: ks = XK_F35; break;
		case NSInsertFunctionKey: ks = XK_Insert; break;
		case NSDeleteFunctionKey: ks = XK_Delete; break;
		case NSHomeFunctionKey: ks = XK_Home; break;
		case NSBeginFunctionKey: ks = XK_Begin; break;
		case NSEndFunctionKey: ks = XK_End; break;
		case NSPageUpFunctionKey: ks = XK_Page_Up; break;
		case NSPageDownFunctionKey: ks = XK_Page_Down; break;
		case NSPrintScreenFunctionKey: ks = NO_MAPPING; break;
		case NSScrollLockFunctionKey: ks = XK_Scroll_Lock; break;
		case NSPauseFunctionKey: ks = XK_Pause; break;
		case NSSysReqFunctionKey: ks = XK_Sys_Req; break;
		case NSBreakFunctionKey: ks = XK_Break; break;
		case NSResetFunctionKey: ks = NO_MAPPING; break;
		case NSStopFunctionKey: ks = XK_Cancel; break;
		case NSMenuFunctionKey: ks = XK_Menu; break;
		case NSUserFunctionKey: ks = NO_MAPPING; break;
		case NSSystemFunctionKey: ks = NO_MAPPING; break;
		case NSPrintFunctionKey: ks = XK_Print; break;
		case NSClearLineFunctionKey: ks = XK_Clear; break;
		case NSClearDisplayFunctionKey: ks = NO_MAPPING; break;
		case NSInsertLineFunctionKey: ks = NO_MAPPING; break;
		case NSDeleteLineFunctionKey: ks = NO_MAPPING; break;
		case NSInsertCharFunctionKey: ks = NO_MAPPING; break;
		case NSDeleteCharFunctionKey: ks = NO_MAPPING; break;
		case NSPrevFunctionKey: ks = XK_Prior; break;
		case NSNextFunctionKey: ks = XK_Next; break;
		case NSSelectFunctionKey: ks = XK_Select; break;
		case NSExecuteFunctionKey: ks = XK_Execute; break;
		case NSUndoFunctionKey: ks = XK_Undo; break;
		case NSRedoFunctionKey: ks = XK_Redo; break;
		case NSFindFunctionKey: ks = XK_Find; break;
		case NSHelpFunctionKey: ks = XK_Help; break;
		case NSModeSwitchFunctionKey: ks = XK_Mode_switch; break;
		case '\t': ks = XK_Tab; break;
		case '\r': ks = XK_Return; break;
		case '\b': ks = XK_BackSpace; break;
		case ASCII_DEL: ks = XK_BackSpace; break;
		case ASCII_ESC: ks = XK_Escape; break;
	}
	
	if(ks && pKeysym) *pKeysym = ks;
	
	return ks != 0;
}

-(void)sendKey:(NSEvent*)event keyState:(enum RFBKeyEventKeyState)keyState
{
	uint32_t keysym = 0;
	
	unsigned int len = [[event characters] length];
	NSString *chars = [event characters];
	
	if(len == 1 && translateUnicharToX11Keysym([chars characterAtIndex:0], &keysym)) {
		NSLog([NSString stringWithFormat:@"sendKey: keyCode: 0x%X => keySym: 0x%X",[event keyCode], keysym]);
		[mRFB sendKeyEvent:keyState keysym:keysym];
	} else if(len >= 1) {
		NSLog([NSString stringWithFormat:@"sendKey: characters: '%@'",[event characters]]);
		const char* str = [[event characters] UTF8String];
		int len = [[event characters] length];
		int i;
		for(i = 0; i < len; ++i) {
			uint32_t keysym = str[i];
			[mRFB sendKeyEvent:keyState keysym:keysym];
		}
	} else {
		NSLog(@"sendKey: DEAD KEY - Ignoring");	
	}
}

- (void)keyDown:(NSEvent *)event
{
	[self sendKey:event keyState:RFBKeyEventKeyStatePressed];
}

- (void)keyUp:(NSEvent *)event
{
	[self sendKey:event keyState:RFBKeyEventKeyStateReleased];
}

static enum RFBKeyEventKeyState
keyStateFromFlags(unsigned int flags, unsigned int keyMask)
{
	NSLog([NSString stringWithFormat:@"keyStateFromFlags: %d", flags & keyMask ? RFBKeyEventKeyStatePressed : RFBKeyEventKeyStateReleased]);
	return flags & keyMask ? RFBKeyEventKeyStatePressed : RFBKeyEventKeyStateReleased;
}

- (void)flagsChanged:(NSEvent *)event
{
	unsigned int newModifiers = [event modifierFlags];
	unsigned int changes = mCurrentModifierFlags ^ newModifiers;
	
	struct FlagToKeysymMap {
		unsigned int keyMask;
		unsigned int keysym;
	};
	
	const struct FlagToKeysymMap map[] = {
		{ NSAlphaShiftKeyMask, XK_Caps_Lock },
		{ NSShiftKeyMask, XK_Shift_L },
		{ NSControlKeyMask, XK_Control_L },
		{ NSAlternateKeyMask, XK_Alt_L },
		{ NSCommandKeyMask, XK_Meta_L },
		// { NSNumericPadKeyMask, NO MAPPING },
		{ NSHelpKeyMask, XK_Help }
		// { NSFunctionKeyMask, NO MAPPING }
	};
	
	const int mapLen = sizeof(map)/sizeof(map[0]);
	int i;

	if(changes) {
		
		NSLog(@"flagsChanged");
		
		for(i = 0; i < mapLen; ++i) {
			if(changes & map[i].keyMask) {
				[mRFB sendKeyEvent:keyStateFromFlags(newModifiers, map[i].keyMask)
							keysym:map[i].keysym];
			}
		}
	
		mCurrentModifierFlags = newModifiers;
	}
}

@end
