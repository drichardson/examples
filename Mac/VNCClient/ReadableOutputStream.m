//
//  ReadableOutputStream.m
//  VNCClient
//
//  Created by Douglas Richardson on 3/23/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.
//

#import "ReadableOutputStream.h"

@implementation ReadableOutputStream

-(id)init
{
	self = [super init];
	if(self) {
		mData = [[NSMutableData alloc] init];
		mPosition = 0;
		mDelegate = self;
	}
	return self;
}

-(void)dealloc
{
	[super dealloc];
}

-(void)appendData:(NSData*)data
{
	[mData appendData:data];
}


// Implement methods required by subclasses of NSStream
- (void)open
{
}

- (void)close
{
}

- (id)delegate
{
	return mDelegate;
}

- (void)setDelegate:(id)delegate
{
	mDelegate = delegate == nil ? self : delegate;
}

- (NSError *)streamError
{
	return nil;
}

- (NSStreamStatus)streamStatus
{
	NSStreamStatus r = NSStreamStatusOpen;
	
	if(![self hasSpaceAvailable]) r = NSStreamStatusAtEnd;
	
	return r;
}

- (void)scheduleInRunLoop:(NSRunLoop *)aRunLoop forMode:(NSString *)mode
{
}

- (void)removeFromRunLoop:(NSRunLoop *)aRunLoop forMode:(NSString *)mode
{
}

- (id)propertyForKey:(NSString *)key
{
	return nil;
}

//Implement methods required by subclasses of NSOutputStream

- (int)write:(const uint8_t *)buffer maxLength:(unsigned int)len
{
	[mData appendBytes:buffer length:len];
	return len;
}

- (BOOL)hasSpaceAvailable
{
	return YES;
}

// Implement methods specific to this class
-(int)readData:(void*)buffer maxBytes:(int)maxlen;
{
	int bytesLeft = [mData length] - mPosition;
	int bytesToRead = MIN(bytesLeft, maxlen);
	
	[mData getBytes:buffer range:NSMakeRange(mPosition,bytesToRead)];
	
	mPosition += bytesToRead;
	
	return bytesToRead;
}

-(NSData*)readData
{
	unsigned bytesLeft = [mData length] - mPosition;
	NSData *d = [mData subdataWithRange:NSMakeRange(mPosition, bytesLeft)];
	mPosition += bytesLeft;
	return d;
}

@end
