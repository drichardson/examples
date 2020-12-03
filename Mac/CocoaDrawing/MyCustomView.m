#import "MyCustomView.h"

@implementation MyCustomView

- (id)initWithFrame:(NSRect)frameRect
{
	if ((self = [super initWithFrame:frameRect]) != nil) {
		// Add initialization code here
	}
	return self;
}

- (void)drawRect:(NSRect)rect
{
	puts("drawRect");
	
	// Draw a background of white.
	NSEraseRect(rect);
#if 1
	//NSColor *aColor = [NSColor colorWithCalibratedRed:0 green:0 blue:1 alpha:0.5];
	//float colorComponents[] = { 1.0, 0, 0, 0.1 }; // RGB and alpha.
	//NSColor *aColor = [NSColor colorWithColorSpace:[NSColorSpace genericRGBColorSpace]
	//									 components:colorComponents
	//										  count:4];
	//NSColor *aColor = [NSColor redColor];
	//[aColor setAlpha:YES];
	//[aColor setAlphaValue:0.0];
	//[[NSColor purpleColor] setFill];
	//NSRectFill(rect);
	//[aColor setFill];
	//[[NSColor whiteColor] setFill];
	//NSRectFill(rect);
	//[[NSColor colorWithCalibratedRed:1 green:1 blue:1 alpha:0 ] setFill];
	//NSRectFill(rect);
	//[[NSColor whiteColor] setFill];
	//NSRectFill(rect);
	//[[NSColor blackColor] setFill];
#endif	
	[NSBezierPath setDefaultLineWidth:3.0];
	
	// The default bezier path appears to only affect the rectangle
	// drawn by the NSBezierPath object and not the NSFrameRect, contrary
	// to Apple documentation.
	[[NSColor magentaColor] setStroke];
	[NSBezierPath strokeRect:NSMakeRect(30.0, 30.0, 15.0, 15.0)];
	[[NSColor cyanColor] setFill];
	NSFrameRect(NSMakeRect(50.0, 30.0, 15.0, 15.0));
	[[NSColor orangeColor] setFill];
	NSFrameRectWithWidth(NSMakeRect(70.0, 30.0, 25.0, 25.0), 5.0);
	
	// NOTE: When the setDefaultLineWidth call is made seems to matter. That is,
	// it must be set before the path object is created. If you set it after the
	// path object is created and before the stroke is made, it will control
	// the line width.
	[NSBezierPath setDefaultLineWidth:3.0];
	//[NSBezierPath setDefaultLineWidth:1.0];
	
	NSBezierPath *aPath = [NSBezierPath bezierPath];
	
	[aPath moveToPoint:NSMakePoint(0.0, 0.0)];
	[aPath lineToPoint:NSMakePoint(10.0, 10.0)];
	[aPath curveToPoint:NSMakePoint(18.0, 21.0)
		  controlPoint1:NSMakePoint(6.0, 2.0)
		  controlPoint2:NSMakePoint(28.0, 10.0)];
	[aPath appendBezierPathWithRect:NSMakeRect(2.0, 16.0, 8.0, 5.0)];
	
	
	[aPath setLineWidth:1.5];
	
	[[NSColor redColor] setStroke];
	[aPath stroke];

	// Use a transform to stroke the same path in another location.
	NSAffineTransform *t = [NSAffineTransform transform];
	[t translateXBy:150.0 yBy:0.0];
	[t scaleBy:2.0];
	[t rotateByDegrees:45.0];
	[t transformBezierPath:aPath];
	[t concat];
	[[NSColor blueColor] setStroke];
	[aPath stroke];
	
	// Invert the transformation matrix to get the CTM
	// (current transformation matrix) back to normal.
	[t invert];
	[t concat];
	
	// Now modify the path directly instead of the CTM.
	t = [NSAffineTransform transform]; // start with a fresh identity matrix.
	[t translateXBy:0.0 yBy:40.0];
	NSBezierPath *aPath2 = [t transformBezierPath:aPath];
	[[NSColor greenColor] setStroke];
	[aPath2 stroke];
	
	t = [NSAffineTransform transform];
	[t translateXBy:0.0 yBy:30.0];
	[t scaleBy:1.5];
	NSBezierPath *aPath3 = [t transformBezierPath:aPath2];
	[[NSColor orangeColor] setStroke];
	[aPath3 stroke];
	
	t = [NSAffineTransform transform];
	[t scaleBy:1.5];
	[t translateXBy:0.0 yBy:30.0];
	NSBezierPath *aPath4 = [t transformBezierPath:aPath2];
	[aPath4 setLineCapStyle:NSRoundLineCapStyle];
	float lineDash[2] = { 1.5, 2.5 };
	[aPath4 setLineDash:lineDash count:2 phase:0.0];
	[[NSColor purpleColor] setStroke];
	[aPath4 stroke];
	
	// Use lines to draw a polygon.
	NSBezierPath *polygon = [NSBezierPath bezierPath];
	[polygon setLineWidth:0];
	[polygon moveToPoint:NSMakePoint(0, 0)];
	[polygon lineToPoint:NSMakePoint(10, 0)];
	[polygon lineToPoint:NSMakePoint(10, 10)];
	[polygon lineToPoint:NSMakePoint(0, 10)];
	[polygon closePath];
	[[NSColor magentaColor] setStroke];
	[[NSColor yellowColor] setFill];
	[polygon stroke];
	[polygon fill];
	
	t = [NSAffineTransform transform];
	[t translateXBy:100 yBy:0];
	[t scaleBy:4];
	polygon = [t transformBezierPath:polygon];
	[[NSColor grayColor] setStroke];
	[polygon stroke];
	
	// Draw some ovals and circles.
	NSBezierPath *oval = [NSBezierPath bezierPathWithOvalInRect:NSMakeRect(200,10,80,120)];
	[oval stroke]; // First stroke, then fill.
	[oval fill];
	
	NSBezierPath *oval2 = [NSBezierPath bezierPathWithOvalInRect:NSMakeRect(300,10,80,120)];
	[oval2 fill]; // First fill, then stroke.
	[oval2 stroke];
	
	NSBezierPath *arc = [NSBezierPath bezierPath];
	[arc moveToPoint:NSMakePoint(400, 0)];
	[arc appendBezierPathWithArcFromPoint:NSMakePoint(450,0) toPoint:NSMakePoint(450,100) radius:50];
	[arc stroke];
	
	NSBezierPath *arc2 = [NSBezierPath bezierPath];
	//[arc2 moveToPoint:NSMakePoint(500, 0)];
	//[arc2 removeAllPoints];
	[arc2 appendBezierPathWithArcWithCenter:NSMakePoint(500, 0) radius:30 startAngle:45 endAngle:135];
	[arc2 stroke];

	// Bezier curves
	NSBezierPath *curve = [NSBezierPath bezierPath];
	[curve moveToPoint:NSMakePoint(400,60)];
	[curve curveToPoint:NSMakePoint(600, 100)
		  controlPoint1:NSMakePoint(525, 145)
		  controlPoint2:NSMakePoint(575, 0)];
	t = [NSAffineTransform transform];
	[t translateXBy:-100 yBy:0];
	[curve transformUsingAffineTransform:t];
	[[NSColor blackColor] setStroke];
	[curve stroke];
	[[NSColor colorWithCalibratedRed:0 green:1 blue:0 alpha:0.5] setFill];
	[curve fill];
	
	// Draw text method 1 using the NSFont object.
	NSFont *font = [NSFont systemFontOfSize:18.0];
	NSGlyph glyph = [font glyphWithName:@"a"];
	NSBezierPath *textPath = [NSBezierPath bezierPath];
	[textPath moveToPoint:NSMakePoint(80, 80)];
	[textPath appendBezierPathWithGlyph:glyph inFont:font];
	[textPath setLineWidth:1];
	[[NSColor blackColor] setFill];
	[textPath fill];
	
	// Draw text method 2 using the text system objects.
	// See Assembling the Text System by Hand in Text System Overview.
	NSTextStorage *textStorage = [[NSTextStorage alloc] init];
	NSLayoutManager *layoutManager = [[NSLayoutManager alloc] init];
	[textStorage addLayoutManager:layoutManager];
	[layoutManager release]; // Release layoutManager because textStorage retained it.
	NSTextContainer *container = [[NSTextContainer alloc] initWithContainerSize:NSMakeSize(150.0 /*w*/, 40.0 /*h*/)];
	[layoutManager addTextContainer:container];
	[container release]; // Release container since layoutManager owns it.
	NSAttributedString *attributedString = [[NSAttributedString alloc] initWithString:@"I like tests. 1234567890"];
	[textStorage setAttributedString:attributedString];
	glyph = [layoutManager glyphAtIndex:0];
	
	textPath = [NSBezierPath bezierPath];
	[textPath moveToPoint:NSMakePoint(160, 80)];
	[textPath appendBezierPathWithGlyph:glyph inFont:font];
	[textPath setLineWidth:1];	
	[textPath fill];
	
	NSRange range;
	range.location = 0;
	range.length = [attributedString length];
	font = [NSFont fontWithName:@"Times" size:25.0];
	
	// Allocate enough memory for glyphRange + 1 elements (according to NSLayoutManager
	// documentation for getGlyphs:range:
	NSGlyph *glyphs = (NSGlyph*)malloc(sizeof(NSGlyph) * (range.length + 1));
	printf("Glyph Range: %d, %d\n", range.location, range.length);
	
	unsigned glyphCount = [layoutManager getGlyphs:glyphs range:range];
	textPath = [NSBezierPath bezierPath];
	[textPath moveToPoint:NSMakePoint(80, 120)];
	[textPath appendBezierPathWithGlyphs:glyphs count:glyphCount inFont:font];
	free(glyphs);
	[textPath fill];
	
	[textStorage release];
	
	// Load and display an image.
	NSString *imageName = [[NSBundle mainBundle] pathForResource:@"image1" ofType:@"JPG"];
	if(imageName != nil) {
		NSImage *tempImage = [[NSImage alloc] initWithContentsOfFile:imageName];
		if(tempImage != nil) {
			//[tempImage drawAtPoint:NSMakePoint(0, 0) fromRect:<#(NSRect)fromRect#> operation:<#(NSCompositingOperation)op#> fraction:<#(float)delta#>];
		} else {
			printf("Error initializing NSImage from main bundle.\n");
		}
	} else {
		printf("Error loading image1 from main bundle\n");
	}
	
	NSImage *tempImage = [NSImage imageNamed:@"Photo 10"];
	if(tempImage != nil) {
		[tempImage setScalesWhenResized:YES];
		NSSize size = NSMakeSize(640/2.5,480/2.5);
		[tempImage setSize:size];
		printf("Origin: %d, %d\n", rect.origin.x, rect.origin.y);
		NSPoint point = NSMakePoint(rect.size.width - size.width + rect.origin.x, rect.size.height - size.height + rect.origin.y);
		[tempImage drawAtPoint:point fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:0.50];
	} else {
		printf("Error loading Photo 10 from resource.\n");
	}
}

@end
