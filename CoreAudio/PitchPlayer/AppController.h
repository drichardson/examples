//
//  AppController.h
//  PitchPlayer
//
//  Created by Doug on 3/18/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "PitchGenerator.h"


@interface AppController : NSObject {
	IBOutlet NSTextField *pitchFrequency;
	PitchGenerator *pitchGenerator;
}

-(IBAction)playPressed:(id)sender;
-(IBAction)stopPressed:(id)sender;

@end
