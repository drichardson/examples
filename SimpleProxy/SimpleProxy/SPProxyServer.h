//
//  SPServer.h
//  SimpleProxy
//
//  Created by Douglas Richardson on 2/11/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>

@protocol SPServerDelegate;

@interface SPProxyServer : NSObject

- (void)start;
- (void)stop;

// Delegate methods may be called on any thread.
@property (nonatomic, assign) id<SPServerDelegate> delegate;

@end

@protocol SPServerDelegate
@required
- (void)proxyServer:(SPProxyServer*)server didReceiveRequest:(NSData*)data forConnection:(NSString*)identifier;
- (void)proxyServer:(SPProxyServer*)server didReceiveResponse:(NSData*)data forConnection:(NSString*)identifier;
- (void)proxyServer:(SPProxyServer*)server fatalErrorDidOccur:(NSError*)error;
@end
