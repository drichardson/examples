//
//  Speaker.h
//  Speech
//
//  Created by Doug on 2/19/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface Speaker : NSObject {
	NSSpeechSynthesizer *synth;
}

- (void)speak:(NSString*)phrase;

@end
