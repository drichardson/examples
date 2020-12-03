//
//  Listener.h
//  Speech
//
//  Created by Doug on 2/19/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "Speaker.h"

@interface Listener : NSObject {
	NSSpeechRecognizer *recognizer;
	Speaker *speaker;
}

@end
