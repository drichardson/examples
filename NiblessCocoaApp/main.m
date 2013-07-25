#include <Cocoa/Cocoa.h>

@interface AppDelegate : NSObject
{
}
@end

@implementation AppDelegate
- (void)applicationDidBecomeActive:(NSNotification *)aNotification
{
	NSLog(@"applicationDidBecomeActive");
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
	NSLog(@"applicationDidFinishLaunching");
}

- (NSMenu *)applicationDockMenu:(NSApplication *)sender
{
	NSLog(@"applicationDockMenu");
	
	NSMenu *menu = [[[NSMenu alloc] initWithTitle:@"Main app menu"] autorelease];
	NSMenuItem *item = [[[NSMenuItem alloc] initWithTitle:@"My Quit" action:@selector(myQuit:) keyEquivalent:@""] autorelease];
	[menu addItem:item];
	
	return menu;
}

-(void)myQuit:(id)sender
{
	NSLog(@"My Quit called");
	[[NSApplication sharedApplication] terminate:self];
}

- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication *)sender
{
	NSLog(@"applicationShouldTerminate");
	return NSTerminateNow;
}

@end

@interface MyView : NSView
@end

@implementation MyView

-(void)drawRect:(NSRect)rect
{
	[[NSColor purpleColor] setFill];
	[[NSColor whiteColor] setStroke];
	
	NSRectFill(rect);
	
	NSBezierPath *path = [NSBezierPath bezierPath];
	[path moveToPoint:NSMakePoint(10, 10)];
	[path lineToPoint:NSMakePoint(70, 70)];
	[path curveToPoint:NSMakePoint(100, 100) controlPoint1:NSMakePoint(70, 70) controlPoint2:NSMakePoint(120, 70)];
	[path setLineWidth:3];
	[path stroke];
}

@end


int main(int argc, const char** argv)
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	
	if(!NSApplicationLoad())
	{
		NSLog(@"NSApplicationLoad failed");
		exit(1);
	}
	
	NSLog(@"Running the main run loop");
	
	NSLog(@"Calling sharedApplication");
	[NSApplication sharedApplication];
	[NSApp setDelegate:[[AppDelegate alloc] init]];
	
	NSLog(@"Setting up the main window");
	
	// Position at the bottom left, but take into account the dock and anything else the windows server may worry about.
	NSRect contentRect = [[NSScreen mainScreen] visibleFrame];
	contentRect.size = NSMakeSize(300, 200);
	
	// The windows style is super cool. You can control what buttons are on the title bar, whether there is a title bar, and
	// whether to use a textured background or not.	
	NSWindow *window = [[NSWindow alloc] initWithContentRect:contentRect
												   styleMask:NSTexturedBackgroundWindowMask //| NSTitledWindowMask//| //NSResizableWindowMask | NSClosableWindowMask
													 backing:NSBackingStoreBuffered
													   defer:NO];
	
	[window setAlphaValue:0.7];
	[window makeKeyAndOrderFront:nil];
	[window setTitle:@"My Nibless App"];
	[window setContentView:[[[MyView alloc] initWithFrame:NSMakeRect(0,0,10,10)] autorelease]];
	
	NSLog(@"Calling NSApp run");
	[NSApp run];
	NSLog(@"NSApp run returned");
	
	[pool release];
	
	return 0;
}

