//
//  TextLimiterFormatter.m
//  Safari Delicious Extension
//
//  Created by Douglas Richardson on 9/16/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.
//

#import "DXTextLimiterFormatter.h"

@implementation DXTextLimiterFormatter

- (id)initWithLimit:(unsigned int)textLimit
{
	if([super init])
	{
		limit = textLimit;
	}
	
	return self;
}

- (BOOL)getObjectValue:(id *)obj forString:(NSString *)string errorDescription:(NSString  **)error
{
	*obj = string;
	return YES;
}

- (NSString *)stringForObjectValue:(id)anObject
{
	if ([anObject isKindOfClass: [NSString class]])
		return anObject;
	return nil;
}

- (BOOL)isPartialStringValid:(NSString *)partialString newEditingString:(NSString **)newString errorDescription:(NSString **)error
{	
	if([partialString length] > limit)
	{
		*newString = [partialString substringWithRange:NSMakeRange(0, limit)];
		return NO;
	}

	*newString = partialString;	
	return YES;
}

@end
