//
//  MultiplicationValueTransformer.h
//  CocoaDrawing1
//
//  Created by Douglas Richardson on 3/13/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MultiplicationValueTransformer : NSValueTransformer {
	double factor;
}
- (id) initWithFactor:(double) newFactor;
@end
