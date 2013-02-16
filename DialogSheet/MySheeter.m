#import "MySheeter.h"

@implementation MySheeter

- (IBAction)cancelPressed:(id)sender
{
	NSLog(@"cancelPressed");
	[NSApp endSheet:sheetWindow];
	[sheetWindow orderOut:nil];
}

- (IBAction)okPressed:(id)sender
{
	NSLog(@"okPressed");
	[NSApp endSheet:sheetWindow];
	[sheetWindow orderOut:nil];
}

-(NSWindow*)mySheetWindow
{
	return sheetWindow;
}

@end
