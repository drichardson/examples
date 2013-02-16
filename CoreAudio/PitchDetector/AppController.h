//
//  AppController.h
//  PitchDetector
//
//  Created by Doug on 3/23/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "DominantFrequencyDetector.h"

@interface AppController : NSObject {
	IBOutlet NSTextField *noteField;
	DominantFrequencyDetector *frequencyDetector;
}

-(IBAction)toggleStartStop:(id)sender;

@end
