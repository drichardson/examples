#import "BrainstormView.h"

@implementation BrainstormView

+(void)initialize
{
	NSLog(@"BrainstormView initialize");
	
	Class class = [self class];
	[class exposeBinding:@"notes"];
}

-(NSMutableArray*)notes
{
	//NSLog(@"notes called: %@", notes);
	
	return [[notes retain] autorelease];
}

-(void)setNotes:(NSMutableArray*)array
{
	//NSLog(@"setNotes called");
	
	if(notes != array)
	{
		[notes release];
		notes = [array retain];
		
		// TODO: Update view.
	}
}

static NSRect
RectFromNode(NSManagedObject* node)
{
	CGFloat width = [[node valueForKey:@"width"] floatValue];
	CGFloat height = [[node valueForKey:@"height"] floatValue];
	CGFloat x = [[node valueForKey:@"xPosition"] floatValue];
	CGFloat y = [[node valueForKey:@"yPosition"] floatValue];
	return NSMakeRect(x, y, width, height);
}

-(void)drawRect:(NSRect)rect
{
	[[NSColor whiteColor] setFill];
	NSRectFill(rect);
	
	//NSLog(@"drawRect *************************************");
	
	// First draw all the connections.
	
	[[NSColor yellowColor] setFill];
	[[NSColor blackColor] setStroke];
	//NSGraphicsContext* aContext = [NSGraphicsContext currentContext];
	[NSBezierPath setDefaultLineWidth:3.0];
	
	// Then draw all the nodes, so they are on top of the connections.
	for(NSManagedObject* node in [self notes])
	{
		NSRect nodeRect = RectFromNode(node);
		NSString *note = [node valueForKey:@"note"];
		//NSLog(@"node: %@, %@", NSStringFromRect(nodeRect), note);
		
		NSRectFill(nodeRect);
		[NSBezierPath strokeRect:nodeRect];
		[note drawInRect:nodeRect withAttributes:nil];
	}
}

-(IBAction)refreshView:(id)sender
{
	NSLog(@"refreshView called");
	[self setNeedsDisplay:YES];
}

@end
