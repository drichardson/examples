//
//  JavaScriptRunner.h
//  JavaScriptCorePlistBridge
//
//  Created by Doug on 1/29/11.
//  Copyright 2011 Douglas Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>

@protocol SecurityContext
- (id)securityContextValueForKey:(NSString*)key;
@end


@interface JavaScriptRunner : NSObject
{
}

- (void)runJavaScript:(NSString*)javascript withSecurityContext:(id <SecurityContext>)securityContext;

@end
