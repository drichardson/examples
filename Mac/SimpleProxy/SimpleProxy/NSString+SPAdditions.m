//
//  NSString+SPAdditions.m
//  SimpleProxy
//
//  Created by Douglas Richardson on 2/13/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import "NSString+SPAdditions.h"

@implementation NSString (SPAdditions)

+ (NSString*)uuid
{
    CFUUIDRef uuid = CFUUIDCreate(NULL);
    NSString* result = (NSString*)CFUUIDCreateString(NULL, uuid);
    CFRelease(uuid);
    return [result autorelease];
}

@end
