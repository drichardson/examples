//
//  SPLog.m
//  SimpleProxy
//
//  Created by Douglas Richardson on 2/12/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import "SPLog.h"
#import <pthread.h>

static int _logLevel;
static void (^_logBlock)(NSString* message);
static NSSet* _enabledComponents;

void SPLogInitialize(int level, NSSet* enabledComponents, void (^logBlock)(NSString* msg))
{
    _logLevel = level;
    _enabledComponents = [enabledComponents copy];
    _logBlock = [logBlock copy];
}

void SPLog(int level, NSString* component, NSString* format, ...)
{
    va_list va;
    va_start(va, format);
    SPLogv(level, component, format, va);
    va_end(va);
}

void SPLogv(int level, NSString* component, NSString* format, va_list va)
{
    if ( level > _logLevel )
    {
        return;
    }
    
    // _enabledComponents == nil means logging disabled for all components.
    if ( _enabledComponents == nil )
    {
        return;
    }
    
    // The empty set means log all.
    BOOL logAllComponents = [_enabledComponents count] == 0;
    
    if ( !logAllComponents && [_enabledComponents member:component] == nil )
    {
        return;
    }
    
    char threadID[40];
    if ( [NSThread isMainThread] )
    {
        strlcpy(threadID, "MAIN", sizeof(threadID));
    }
    else
    {
        snprintf(threadID, sizeof(threadID), "%p", pthread_self());
    }
    
    NSString* msg = [[NSString alloc] initWithFormat:format arguments:va];
    NSString* outputString = [[NSString alloc] initWithFormat:@"[%s] %@: %@", threadID, component, msg];
    _logBlock(outputString); // SPSetLogOutput must be called before any logs are issued.
    [outputString release];
    [msg release];
}

void SPLogClass(int level, id obj, NSString* format, ...)
{    
    va_list va;
    va_start(va, format);
    SPLogv(LOG_INFO, NSStringFromClass([obj class]), format, va);
    va_end(va);
}
