#import <Cocoa/Cocoa.h>

@interface BrainstormView : NSView
{
	NSMutableArray *notes;
}

@property(retain, readwrite) NSMutableArray *notes;

// Debug
-(IBAction)refreshView:(id)sender;

@end
