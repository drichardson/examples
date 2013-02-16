//
//  MutableDictionary.m
//  JavaScriptCorePlistBridge
//
//  Created by Doug on 1/30/11.
//  Copyright 2011 Douglas Richardson. All rights reserved.
//

#import "MutableDictionary.h"

static void _PlistDictionaryInitialize(JSContextRef ctx, JSObjectRef object)
{
	printf("Initialize called on %p\n", object);
}

static void _PlistDictionaryFinalize(JSObjectRef object)
{
	printf("Finalize called on %p\n", object);
}

void DefineMutableDictionaryClass(JSContextRef ctx)
{
	JSClassDefinition def = kJSClassDefinitionEmpty;
	
	def.className = "PlistDictionary";
	def.initialize = _PlistDictionaryInitialize;
	def.finalize = _PlistDictionaryFinalize;
	
	JSClassRef class = JSClassCreate(&def);
	JSObjectRef globalObject = JSContextGetGlobalObject(ctx);
	JSObjectRef constructor = JSObjectMakeConstructor(ctx, class, NULL);
	JSStringRef ctorName = JSStringCreateWithUTF8CString("mydict");
	JSValueRef exception = NULL;
	JSObjectSetProperty(ctx, globalObject, ctorName, constructor, kJSPropertyAttributeNone, &exception);
	
	if ( exception != NULL )
	{
		printf("Failed to create plist dictionary class\n");
	}
	
	JSClassRelease(class);
	JSStringRelease(ctorName);
}
