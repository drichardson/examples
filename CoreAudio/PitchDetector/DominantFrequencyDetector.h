//
//  DominantFrequencyDetector.h
//  DominantFrequencyDetector
//
//  Created by Doug on 3/23/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#include <CoreServices/CoreServices.h>

#define kDominantFrequencyDetector_FrequencyChangedNotification @"DominantFrequencyDetector_FrequencyChangedNotification"

@interface DominantFrequencyDetector : NSObject {
	NSNumber *dominantFrequencyInHz;
	BOOL mRunDectectorThread;
	BOOL mDetectorThreadIsFinished;
	MPSemaphoreID detectorThreadRunningSempahore;
}

@property (retain, readonly) NSNumber* dominantFrequencyInHz;

-(void)startDetecting;
-(void)stopDetecting;

@end
