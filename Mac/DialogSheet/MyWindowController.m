#import "MyWindowController.h"

@implementation MyWindowController

- (id)init
{
	if([super init])
	{
		mySheeter = [[MySheeter alloc] init];
		if(mySheeter)
			[NSBundle loadNibNamed:@"SheetNIB" owner:mySheeter];
	}
	return self;
}

- (void)dealloc
{
	[mySheeter release];
	[super dealloc];
}

- (IBAction)showTheSheet:(id)sender
{
	NSLog(@"showTheSheet");
	[NSApp beginSheet:[mySheeter mySheetWindow] modalForWindow:[NSApp mainWindow] modalDelegate:self didEndSelector:nil contextInfo:NULL];
}

@end
