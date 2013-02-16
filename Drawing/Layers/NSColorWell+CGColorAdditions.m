//
//  NSColorWell+CGColorAdditions.m
//  Layers
//
//  Created by Doug on 7/12/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import "NSColorWell+CGColorAdditions.h"
#import "NSColor+CGColorAdditions.h"

@implementation NSColorWell (CGColorAdditions)

- (CGColorRef)cgColorRef
{
	return [[self color] cgColorRef];
}

@end
