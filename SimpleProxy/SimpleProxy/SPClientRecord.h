//
//  SPClientRecord.h
//  SimpleProxy
//
//  Created by Douglas Richardson on 2/14/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SPClientRecord : NSObject
@property (readonly) NSString* identifier;
@property (nonatomic, assign) int fdServer; // Connection to server. This object takes ownership of the socket.
@property (nonatomic, assign) int fdClient; // Connection to client. This objects takes ownership of the socket.
@property (readonly) NSData* request;
//@property (readonly) NSData* response;
@property (readonly) NSString* host;

- (void)addRequestData:(NSData*)data;
- (void)closeSockets;

@end
