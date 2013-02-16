//
//  PitchGenerator.h
//  PitchPlayer
//
//  Created by Doug on 3/19/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface PitchGenerator : NSObject {
	NSNumber* frequencyInHz;
	BOOL mDone;
	BOOL mThreadFinished;
}

@property (retain) NSNumber* frequencyInHz;

-(void)startPlaying;
-(void)stopPlaying;

@end
