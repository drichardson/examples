//
//  OldSchoolPool.m
//  WeakExample
//
//  Created by Douglas Richardson on 6/20/13.
//

#import "OldSchoolPool.h"

static NSAutoreleasePool* pool;

void OldSchoolPoolMake()
{
    assert(pool == nil);
    pool = [[NSAutoreleasePool alloc] init];
}

void OldSchoolPoolDrain()
{
    [pool release];
}
