//
//  main.m
//  MallocZoneTest
//
//  Created by Douglas Richardson on 8/3/13.
//  Copyright (c) 2013 upthere. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <malloc/malloc.h>

static void zoneLog(char const* msg, void const* ptr)
{
    printf("\tzone=%-18p ptr=%-18p %s\n", malloc_zone_from_ptr(ptr), ptr, msg);
}

int main(int argc, const char * argv[])
{
    @autoreleasepool {
        
        //
        // data section
        //
        puts("data section");
        zoneLog("string literal", "This is a test");
        
        //
        // stack
        //
        puts("stack");
        int x = strlen("1234");
        zoneLog("&x", &x);
        
        //
        // mallocs
        //
        puts("mallocs");
        char* plainMalloc = malloc(100);
        malloc_zone_t* customZone = malloc_create_zone(1*1024*1024, 0);
        char* customZoneMalloc = malloc_zone_malloc(customZone, 100);
        
        zoneLog("plainMalloc", plainMalloc);
        zoneLog("customZoneMalloc", customZoneMalloc);
        
        //
        // ObjC objects.
        //
        NSZone* nsZone = NSCreateZone(1*1024*1024, 128, NO);
        NSMutableDictionary* d_defaultZone = [[NSMutableDictionary alloc] init];
        NSMutableDictionary* d_zone1 = [[NSMutableDictionary allocWithZone:nsZone] init];
        
        puts("ObjC objects");
        zoneLog("d_defaultZone", (__bridge const void *)(d_defaultZone));
        zoneLog("d_zone1", (__bridge const void *)(d_zone1));
        
        
        //
        // ObjC literals
        //
        puts("ObjC literals");
        zoneLog("literal NSString", @"testing");
        zoneLog("literal NSNumber", (__bridge const void *)(@(0xfeedface)));
    }
    
    return 0;
}

