//
//  DRURLProtocol.m
//  ProtocolHandler
//
//  Created by Douglas Richardson on 7/11/13.
//  Copyright (c) 2013 upthere. All rights reserved.
//

#import "DRURLProtocol.h"

@implementation DRURLProtocol

+ (BOOL)canInitWithRequest:(NSURLRequest *)request
{
    return YES; //[[[request URL] scheme] isEqual:@"dr"];
}

+ (NSURLRequest *)canonicalRequestForRequest:(NSURLRequest *)request
{
    return request;
}

+ (BOOL)requestIsCacheEquivalent:(NSURLRequest *)aRequest toRequest:(NSURLRequest *)bRequest
{
    return [super requestIsCacheEquivalent:aRequest toRequest:bRequest];
}

- (id)initWithRequest:(NSURLRequest *)request cachedResponse:(NSCachedURLResponse *)cachedResponse client:(id < NSURLProtocolClient >)client
{
    NSLog(@"Creating DRURLProtocol instance\n\tRequest: URL: %@, Headers: %@", [request URL], [request allHTTPHeaderFields]);
    self = [super initWithRequest:request cachedResponse:cachedResponse client:client];
    if (self) {
        // custom initialization here
    }
    return self;
}

- (void)startLoading
{
    NSLog(@"startLoading called");
    NSString* path =[[NSBundle mainBundle] pathForResource:@"sample_iPod" ofType:@".m4v"];
    NSFileManager* mgr = [NSFileManager new];
    NSDictionary* attrs = [mgr attributesOfItemAtPath:path error:nil];
    assert(attrs);
//    NSURLResponse* response = [[NSURLResponse alloc] initWithURL:[[self request] URL] MIMEType:@"video/m4v" expectedContentLength:[attrs fileSize] textEncodingName:nil];
    
    double delayInSeconds = 0.0;
    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(delayInSeconds * NSEC_PER_SEC));
    dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
        
        NSString* byteRange = [[self request] allHTTPHeaderFields][@"Range"];
        NSLog(@"Byte range is %@", byteRange);
        byteRange = [byteRange stringByReplacingOccurrencesOfString:@"bytes=" withString:@""];
        NSArray* rangeArray = [byteRange componentsSeparatedByString:@"-"];
        NSData* d = [NSData dataWithContentsOfFile:path];
        int totalLength = [d length];
        int begin = 0;
        int end = 0;
        if ( [rangeArray count] == 2 ) {
            begin = [rangeArray[0] intValue];
            end = [rangeArray[1] intValue];
            assert(begin < end);
            assert(end < [d length]);
            // byte
            d = [d subdataWithRange:NSMakeRange(begin, end - begin + 1)];
        }
        else {
            NSLog(@"NOT READY FOR THIS");
        }
        
        NSDictionary* headers = @{
                                  @"Content-Type": @"video/mp4",
                                  @"Content-Length": [@([d length]) stringValue],
                                  @"Accept-Ranges": @"bytes",
                                  @"Accept-Encoding": @"identity",
                                  @"Connection": @"keep-alive",
                                  @"Content-Range": [NSString stringWithFormat:@"bytes %d-%d/%d", begin, end, totalLength]
                                  };
        NSLog(@"Response header is %@", headers);
        NSHTTPURLResponse* response = [[NSHTTPURLResponse alloc] initWithURL:[[self request] URL] statusCode:206 HTTPVersion:@"HTTP/1.1" headerFields:headers];
        // NSHTTPURLResponse* response = [[NSHTTPURLResponse alloc] initWithURL:[[self request] URL] statusCode:200 HTTPVersion:@"HTTP/1.1" headerFields:nil];
        [[self client] URLProtocol:self didReceiveResponse:response cacheStoragePolicy:NSURLCacheStorageAllowed];
        
        if (d) {
            NSLog(@"Saying it worked");
//            [[self client] URLProtocol:self didReceiveResponse:response cacheStoragePolicy:NSURLCacheStorageAllowed];
            [[self client] URLProtocol:self didLoadData:d];
            [[self client] URLProtocolDidFinishLoading:self];
        } else {
            NSLog(@"Saying it failed");
            NSError* error = [NSError errorWithDomain:@"DougDomain" code:123 userInfo:nil];
            [[self client] URLProtocol:self didFailWithError:error];
        }
    });
}

- (void)stopLoading
{
    // Not implemented
    NSLog(@"stopLoading called");
}

@end
