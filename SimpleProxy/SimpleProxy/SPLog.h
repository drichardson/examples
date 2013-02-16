//
//  SPLog.h
//  SimpleProxy
//
//  Created by Douglas Richardson on 2/12/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <stdarg.h>
#import <syslog.h>

// Initialize the log library before calling any of the other functions. This function should
// only be called exactly one time during the execution of a program.
// level - The log level to use as a cutoff
// enabledComponents - The set of enabled components. Pass nil for none or the empty set for all.
void SPLogInitialize(int level, NSSet* enabledComponents, void (^logBlock)(NSString* msg));

// Log a message.
void SPLog(int level, NSString* component, NSString* format, ...);
void SPLogv(int level, NSString* component, NSString* format, va_list va);

// Uses the object's class as the component.
void SPLogClass(int level, id obj, NSString* format, ...) NS_FORMAT_FUNCTION(3,4);

