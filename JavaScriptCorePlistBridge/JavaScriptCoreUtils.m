//
//  JavaScriptCoreUtils.m
//  JavaScriptCorePlistBridge
//
//  Created by Doug on 1/30/11.
//  Copyright 2011 Douglas Richardson. All rights reserved.
//

#import "JavaScriptCoreUtils.h"

NSString* NSStringFromJSString(JSStringRef string)
{
	if ( string == NULL )
	{
		return nil;
	}
	
	NSString* s = (NSString*)JSStringCopyCFString(NULL, string);
	[s autorelease];
	return s;
}

NSString* NSStringFromJSValue(JSContextRef context, JSValueRef value)
{
	if ( value == NULL )
	{
		return nil;
	}
	
	JSStringRef string = JSValueToStringCopy(context, value, NULL);
	NSString* result = NSStringFromJSString(string);
	if ( string )
	{
		JSStringRelease(string);
	}
	return result;
}
