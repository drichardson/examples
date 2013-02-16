//
//  RFBView.h
//  Flash Beep Prototype
//
//  Created by Doug on 5/29/10.
//  Copyright 2010 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <QuartzCore/QuartzCore.h>

@interface RFBView : NSView {
	CALayer *rfbLayer;
	CALayer *flashLayer;
}

- (void)flash;

@end
