//
//  NSColor+CGColorAdditions.m
//  Layers
//
//  Created by Doug on 7/12/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import "NSColor+CGColorAdditions.h"


@interface CGColorRefWrapper : NSObject
{
	@public
	CGColorRef _colorRef;
}
- (id)initWithNSColor:(NSColor*)color;
@end


@implementation NSColor (CGColorAdditions)

- (CGColorRef)cgColorRef
{
	CGColorRefWrapper *wrapper = [[[CGColorRefWrapper alloc] initWithNSColor:self] autorelease];
	return wrapper->_colorRef;
}

@end


// Utility function to convert from NSColor to CGColorRef
static CGColorRef CreateCGColorFromNSColor(NSColor * color)
{
	NSColor * rgbColor = [color colorUsingColorSpace:[NSColorSpace genericRGBColorSpace]];
	CGFloat r, g, b, a;
	[rgbColor getRed:&r green:&g blue:&b alpha:&a];
	return CGColorCreateGenericRGB(r, g, b, a);
}

@implementation CGColorRefWrapper

- (id)initWithNSColor:(NSColor*)color
{
	self = [super init];
	if(self)
	{
		_colorRef = CreateCGColorFromNSColor(color);
		if(_colorRef == NULL)
		{
			[self release];
			self = nil;
		}
	}
	
	return self;
}

- (void)dealloc
{
	if(_colorRef)
		CGColorRelease(_colorRef);
	
	[super dealloc];
}

@end