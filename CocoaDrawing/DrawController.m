//
//  DrawController.m
//  CocoaDrawing1
//
//  Created by Douglas Richardson on 3/13/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "DrawController.h"
#import <WebKit/WebView.h>
#import <WebKit/WebFrame.h>


@implementation DrawController

+ (void) initialize
{
}

- init {
	self = [super init];
	puts("init called");
	if(self) {
		mTransform = [[NSAffineTransform transform] retain];
		[self setURL:@"http://www.google.com"];
		[self setWebPercentageLoaded:0];
		
		[[NSNotificationCenter defaultCenter] addObserver:self
												 selector:@selector(webViewBeginLoad:)
													 name:@"WebProgressStartedNotification"
												   object:webView];
		[[NSNotificationCenter defaultCenter] addObserver:self
												 selector:@selector(webViewLoadProgress:)
													 name:@"WebProgressEstimateChangedNotification"
												   object:webView];
		[[NSNotificationCenter defaultCenter] addObserver:self
												 selector:@selector(webViewEndLoad:)
													 name:@"WebProgressFinishedNotification"
												   object:webView];
	}
	
	return self;
}

- (void) dealloc {
	[mTransform release];
	
	// Remove self as an observer from any and all notification
	// that self may be observing.
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	
	[super dealloc];
}

- (void) webViewBeginLoad:(NSNotification*)notification
{
	//printf("BEGIN: desc: %s, estimated: %f\n", [[notification description] UTF8String], [webView estimatedProgress]);
	[self setWebPercentageLoaded:[webView estimatedProgress]];
}

- (void) webViewLoadProgress:(NSNotification*)notification
{
	//printf("PROG: desc: %s, estimated: %f\n", [[notification description] UTF8String], [webView estimatedProgress]);
	[self setWebPercentageLoaded:[webView estimatedProgress]];
}

- (void) webViewEndLoad:(NSNotification*)notification
{
	//printf("END: desc: %s, estimated: %f\n", [[notification description] UTF8String], [webView estimatedProgress]);
	[self setWebPercentageLoaded:1.0];
}

- (float) argument1 { return mArgument1; }
- (void) setArgument1:(float)newArgument1 { mArgument1 = newArgument1; }
- (float) argument2 { return mArgument2; }
- (void) setArgument2:(float)newArgument2 { mArgument2 = newArgument2; }
-(NSString*) URL { return [[mURLString retain] autorelease]; }
-(void) setURL:(NSString*)newURL
{
	if(newURL != mURLString) {
		[mURLString release];
		mURLString = [newURL copy];
	}
}
- (float) webPercentageLoaded { return mWebPercentageLoaded; }
- (void) setWebPercentageLoaded:(float)newWebPercentageLoaded { mWebPercentageLoaded = newWebPercentageLoaded; }

- (void) appendInfoText:(NSString*) string
{
	// Setup the range to insert the string at the end of the rest of the text in the NSTextView.
	NSRange range;
	range.length = [string length];
	range.location = [[textView textStorage] length];
	
	// Convert the string to an NSAttributedString.
	NSAttributedString *astr = [[NSAttributedString alloc] initWithString:string];
	[[textView textStorage] insertAttributedString:astr atIndex:range.location];
	[astr release];
	
	// Scroll the the newly added text.
	[textView scrollRangeToVisible:range];
}

- (void) printTransform
{
	NSAffineTransformStruct ats = [mTransform transformStruct];
	NSString *s = [NSString stringWithFormat:@"{m11=%f, m12=%f, m21=%f, m22=%f, tX=%f, tY=%f}\n",
		ats.m11, ats.m12, ats.m21, ats.m22, ats.tX, ats.tY];
	printf("Arguments: %f, %f\n", [self argument1], [self argument2]);
	
	[self appendInfoText:s];
}

- (IBAction) printTransform:(id) sender
{
	[self printTransform];
}

- (IBAction) invertTransform:(id) sender
{
	[mTransform invert];
	[self printTransform];
}

- (IBAction) rotateTransformByDegrees:(id) sender
{
	[mTransform rotateByDegrees:[self argument1]];
	[self printTransform];
}

- (IBAction) rotateTransformByRadians:(id) sender
{
	[mTransform rotateByRadians:[self argument1]];
	[self printTransform];
}

- (IBAction) scaleTransform:(id) sender
{
	[mTransform scaleBy:[self argument1]];
	[self printTransform];
}

- (IBAction) scaleXByYByinvertTransform:(id) sender
{
	[mTransform scaleXBy:[self argument1] yBy:[self argument2]];
	[self printTransform];
}

- (IBAction) translateXByYByTransform:(id) sender
{
	[mTransform translateXBy:[self argument1] yBy:[self argument2]];
	[self printTransform];
}

- (IBAction) goToURL:(id) sender
{
	// Add web view notifications.
	// WebViewProgressFinishedNotification
	// WebViewProgressStartedNotification
	// WebViewProgressEstimateChangedNotification
	// Note: the documentation appears to be incorrect regarding the naming of
	// the WebView notifications. The documentation lists the names as WebViewProgress...
	// but the actual names are WebProgress...
	
	[[webView mainFrame] loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:[self URL]]]];
	printf("Returned\n");
}
@end
