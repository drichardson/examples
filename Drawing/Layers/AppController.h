//
//  AppController.h
//  Layers
//
//  Created by Doug on 7/11/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import <AppKit/AppKit.h>
#import <QuartzCore/QuartzCore.h>
#import "MyOpenGLLayer.h"

@interface AppController : NSView
{
	IBOutlet NSView *_mainView;
	CALayer *_rootLayer, *_solidLayer;
	CATextLayer *_textLayer;
	MyOpenGLLayer *_openGLLayer;

	// Solid Layer Outlets
	IBOutlet NSColorWell *_solidLayerBackgroundColorWell;
	IBOutlet NSTextField *_solidLayerAnchorX, *_solidLayerAnchorY;
	IBOutlet NSTextField *_solidLayerPositionX, *_solidLayerPositionY;
	IBOutlet NSTextField *_solidLayerBoundsOriginX, *_solidLayerBoundsOriginY;
	IBOutlet NSTextField *_solidLayerBoundsWidth, *_solidLayerBoundsHeight;
	IBOutlet NSTextField *_solidLayerRotationDegrees;
    IBOutlet NSButton *_solidLayerAnimatePosition;
    IBOutlet NSButton *_solidLayerAnimateRotation;
	
	// Text Layer Outlets
	IBOutlet NSTextField *_textLayerText;
	IBOutlet NSPopUpButton *_textLayerFontName;
	IBOutlet NSTextField *_textLayerFontSize;
	
	// OpenGL Layer Outlets
	IBOutlet NSButton *_openGLLayerIsAsynchronous;
}

- (IBAction)updateSolidLayer:(id)sender;
- (IBAction)updateSolidLayerAnimations:(id)sender;
- (IBAction)updateTextLayer:(id)sender;
- (IBAction)updateOpenGLLayer:(id)sender;

@end
