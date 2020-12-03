//
//  LogObject.m
//  JavaScriptCorePlistBridge
//
//  Created by Doug on 1/30/11.
//  Copyright 2011 Douglas Richardson. All rights reserved.
//

#import "LogObject.h"
#import "JavaScriptCoreUtils.h"

static JSValueRef _LogObjectLogInternal(JSContextRef ctx, NSString* level, size_t argumentCount, const JSValueRef arguments[], JSValueRef* exception)
{
	NSMutableArray* convertedArguments = [[NSMutableArray alloc] initWithCapacity:argumentCount];
	
	for(int i = 0; i < argumentCount; i++)
	{
		NSString* s = NSStringFromJSValue(ctx, arguments[i]);
		if ( s )
		{
			[convertedArguments addObject:s];
		}
	}
	
	NSLog(@"%@: %@", level, [convertedArguments componentsJoinedByString:@" "]);
	
	[convertedArguments release];
	
	return JSValueMakeUndefined(ctx);
}

static JSValueRef _LogObjectDebug(JSContextRef ctx, JSObjectRef function, JSObjectRef thisObject, size_t argumentCount, const JSValueRef arguments[], JSValueRef* exception)
{
	return _LogObjectLogInternal(ctx, @"DEBUG", argumentCount, arguments, exception);
}

static JSValueRef _LogObjectWarn(JSContextRef ctx, JSObjectRef function, JSObjectRef thisObject, size_t argumentCount, const JSValueRef arguments[], JSValueRef* exception)
{
	return _LogObjectLogInternal(ctx, @"WARN", argumentCount, arguments, exception);
}

static JSValueRef _LogObjectError(JSContextRef ctx, JSObjectRef function, JSObjectRef thisObject, size_t argumentCount, const JSValueRef arguments[], JSValueRef* exception)
{
	return _LogObjectLogInternal(ctx, @"ERROR", argumentCount, arguments, exception);
}

void MakeGlobalLogObjectWithName(JSContextRef ctx, const char* logObjectName)
{
	JSObjectRef logObject = JSObjectMake(ctx, NULL, NULL);
	
	// Create a log object with function properties for different logging levels: debug, warn, error.
	JSStringRef name = JSStringCreateWithUTF8CString("debug");
	JSObjectSetProperty(ctx, logObject, name, JSObjectMakeFunctionWithCallback(ctx, name, _LogObjectDebug), kJSPropertyAttributeNone, NULL);
	JSStringRelease(name);
	name = NULL;
	
	name = JSStringCreateWithUTF8CString("warn");
	JSObjectSetProperty(ctx, logObject, name, JSObjectMakeFunctionWithCallback(ctx, name, _LogObjectWarn), kJSPropertyAttributeNone, NULL);
	JSStringRelease(name);
	name = NULL;
	
	name = JSStringCreateWithUTF8CString("error");
	JSObjectSetProperty(ctx, logObject, name, JSObjectMakeFunctionWithCallback(ctx, name, _LogObjectError), kJSPropertyAttributeNone, NULL);
	JSStringRelease(name);
	name = NULL;
	
	// Add the log object to the global object
	name = JSStringCreateWithUTF8CString(logObjectName);
	JSObjectRef globalObject = JSContextGetGlobalObject(ctx);
	JSObjectSetProperty(ctx, globalObject, name, logObject, kJSPropertyAttributeDontDelete, NULL);
	JSStringRelease(name);
	name = NULL;
}
