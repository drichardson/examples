//
//  MyView.h
//  OffscreenDrawing
//
//  Created by Douglas Richardson on 3/29/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MyView : NSView {
	NSBitmapImageRep *mRep;
	uint32_t mNewColor;
}

- (IBAction) copyNewRect:(id) sender;
- (IBAction) makeNewRectColor:(id) sender;
@end
