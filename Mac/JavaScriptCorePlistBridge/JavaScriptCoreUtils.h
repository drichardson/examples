//
//  JavaScriptCoreUtils.h
//  JavaScriptCorePlistBridge
//
//  Created by Doug on 1/30/11.
//  Copyright 2011 Douglas Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <JavaScriptCore/JavaScriptCore.h>

NSString* NSStringFromJSString(JSStringRef string);
NSString* NSStringFromJSValue(JSContextRef context, JSValueRef value);
