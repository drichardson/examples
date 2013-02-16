//
//  DrawController.h
//  CocoaDrawing1
//
//  Created by Douglas Richardson on 3/13/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class WebView;

@interface DrawController : NSObject {
	NSAffineTransform *mTransform;
	IBOutlet NSTextView *textView;
	IBOutlet WebView *webView;
	IBOutlet NSOpenGLView *openGLView;
	float mArgument1;
	float mArgument2;
	NSString *mURLString;
	float mWebPercentageLoaded;
}

- (void) webViewBeginLoad:(NSNotification*)notification;

- (float) argument1;
- (void) setArgument1:(float)newArgument1;
- (float) argument2;
- (void) setArgument2:(float)newArgument2;
-(NSString*) URL;
-(void) setURL:(NSString*)newURL;
- (float) webPercentageLoaded;
- (void) setWebPercentageLoaded:(float)newWebPercentageLoaded;


- (IBAction) printTransform:(id) sender;
- (IBAction) invertTransform:(id) sender;
- (IBAction) rotateTransformByDegrees:(id) sender;
- (IBAction) rotateTransformByRadians:(id) sender;
- (IBAction) scaleTransform:(id) sender;
- (IBAction) scaleXByYByinvertTransform:(id) sender;
- (IBAction) translateXByYByTransform:(id) sender;

- (IBAction) goToURL:(id) sender;
@end
