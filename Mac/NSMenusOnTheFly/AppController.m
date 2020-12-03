#import "AppController.h"

@interface AppController (private)

-(void)menuNeedsUpdate:(NSMenu*)menu;

@end

@implementation AppController

- (IBAction)addMenu:(id)sender {
	NSMenuItem *newMenuItem = [[[NSMenuItem alloc] initWithTitle:@"some menu" action:NULL keyEquivalent:@""] autorelease];
	
	NSMenu *myMenu = [[[NSMenu alloc] initWithTitle:@"Menu Under Test"] autorelease];
	[newMenuItem setSubmenu:myMenu];
	
	NSMenuItem *item1 = [myMenu addItemWithTitle:@"On the fly menu" action:NULL keyEquivalent:@""];
	
	NSMenu *onTheFlyMenu = [[[NSMenu alloc] initWithTitle:@"On the fly menu"] autorelease];
	[onTheFlyMenu setDelegate:self];
	
	[item1 setSubmenu:onTheFlyMenu];
	
	//NSMenu *newMenu = [[[NSMenu alloc] init] autorelease];
	
	[[NSApp mainMenu] addItem:newMenuItem];
}

// NSMenu delegate
-(void)menuNeedsUpdate:(NSMenu*)menu
{
	NSLog(@"menuNeedsUpdate called: item count = %d", [menu numberOfItems]);
	
	if([menu numberOfItems] == 0)
	{
		NSMenuItem *item = [[[NSMenuItem alloc] initWithTitle:@"Add another menu" action:@selector(addMenu:) keyEquivalent:@""] autorelease];
		[item setTarget:self];
		
		[menu addItem:item];
		
		NSMenu *subMenu = [[[NSMenu alloc] init] autorelease];
		item = [[[NSMenuItem alloc] initWithTitle:@"Something in here" action:NULL keyEquivalent:@""] autorelease];
		[item setSubmenu:subMenu];
		[subMenu setDelegate:self];
		[menu addItem:item];
	}
}

@end
