//
//  JavaScriptRunner.m
//  JavaScriptCorePlistBridge
//
//  Created by Doug on 1/29/11.
//  Copyright 2011 Douglas Richardson. All rights reserved.
//

#import "JavaScriptRunner.h"
#import <JavaScriptCore/JavaScriptCore.h>
#import "JavaScriptCoreUtils.h"

#import "LogObject.h"
#import "MutableDictionary.h"

@interface JavaScriptRunner ()

- (void)_setupJavaScriptEnvironment;

@property (assign) JSGlobalContextRef context;

@end


@implementation JavaScriptRunner

@synthesize context=_context;

- (id) init
{
	self = [super init];
	if (self != nil)
	{
		[self _setupJavaScriptEnvironment];
		
		if ( _context == NULL )
		{
			NSLog(@"Error setting up JavaScript execution environment");
			[self release];
			return nil;
		}
	}
	return self;
}

- (void) dealloc
{
	if ( _context )
	{
		JSGlobalContextRelease(_context);
	}
	
	[super dealloc];
}


- (void)runJavaScript:(NSString*)javascript withSecurityContext:(id <SecurityContext>)securityContext
{
	if ( javascript == nil )
	{
		NSLog(@"javascript argument is nil");
		return;
	}
	
	NSAutoreleasePool* pool = [NSAutoreleasePool new];
	
	JSStringRef scriptString = JSStringCreateWithCFString((CFStringRef)javascript);
	
	if ( scriptString )
	{
		JSValueRef exception = NULL;
		JSValueRef result = JSEvaluateScript(_context, scriptString, NULL, NULL, 1, &exception);
		if ( result )
		{
			printf("Script executed successfully\n");
		}
		else
		{
			NSLog(@"Exception: %@", NSStringFromJSValue(_context, exception));
		}
		
		JSStringRelease(scriptString);
	}
	
	[pool release];
}

#pragma mark -
#pragma mark JavaScript Execution Enviornment Setup

- (void)_setupJavaScriptEnvironment
{
	if ( _context != NULL )
	{
		[NSException raise:@"AlreadyInitialized" format:@"JavaScript execution environment is already initialized."];
	}
	
	_context = JSGlobalContextCreateInGroup(NULL, NULL);
	
	MakeGlobalLogObjectWithName(_context, "log");
	DefineMutableDictionaryClass(_context);
}

@end

