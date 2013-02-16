//
//  SPAppDelegate.m
//  SimpleProxy
//
//  Created by Douglas Richardson on 2/11/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import "SPAppDelegate.h"
#import "SPLog.h"

@implementation SPAppDelegate
{
    SPProxyServer* _server;
}

@synthesize window = _window;

- (void)dealloc
{
    _server.delegate = nil;
    [_server release];
    [super dealloc];
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    _server = [SPProxyServer new];
    _server.delegate = self;
    [_server start];
}

- (void)applicationWillTerminate:(NSNotification *)notification
{
}

- (void)proxyServer:(SPProxyServer*)server didReceiveRequest:(NSData*)data forConnection:(NSString*)identifier
{
    SPLogClass(LOG_DEBUG, self, @"HTTP REQUEST %@, len=%d", identifier, [data length]);
}

- (void)proxyServer:(SPProxyServer*)server didReceiveResponse:(NSData*)data forConnection:(NSString*)identifier
{
    SPLogClass(LOG_DEBUG, self, @"HTTP RESPONSE %@, len=%d", identifier, [data length]);
}

- (void)proxyServer:(SPProxyServer*)server fatalErrorDidOccur:(NSError*)error
{
    SPLogClass(LOG_ERR, self, @"Proxy server got an error. %@", error);
}

@end
