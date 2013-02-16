//
//  MultiplicationValueTransformer.m
//  CocoaDrawing1
//
//  Created by Douglas Richardson on 3/13/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "MultiplicationValueTransformer.h"


@implementation MultiplicationValueTransformer

+ (Class)transformedValueClass
{
	return [NSNumber class];
}

+ (BOOL)allowsReverseTransformation
{
	return YES;
}

- (id) init
{
	return [self initWithFactor:2];
}

- (id) initWithFactor:(double) newFactor
{
	self = [super init];
	if(self != nil) {
		factor = newFactor;
	}
	return self;
}

- (id)transformedValue:(id)value
{
	float x;
	
	// Try to get a float value from the value object.
	if(value == nil) return nil;
	
	if([value respondsToSelector:@selector(floatValue)]) {
		x = [value floatValue];
	} else {
		[NSException raise: NSInternalInconsistencyException
					format: @"Value (%@) does not respond to -floatValue.", [value class]];
	}
	
	// Perform multiplication.
	x *= factor;
	return [NSNumber numberWithFloat:x];
}

- (id)reverseTransformedValue:(id)value
{
	float x;
	
	// Try to get a float value from the value object.
	if(value == nil) return nil;
	
	if([value respondsToSelector:@selector(floatValue)]) {
		x = [value floatValue];
	} else {
		[NSException raise: NSInternalInconsistencyException
					format: @"Value (%@) does not respond to -floatValue.", [value class]];
	}
	
	// Perform division.
	x /= factor;
	return [NSNumber numberWithFloat:x];
}

@end
