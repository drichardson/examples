//
//  InjectableInputStream.m
//  VNCClient
//
//  Created by Douglas Richardson on 3/22/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.
//

#import "InjectableInputStream.h"


@implementation InjectableInputStream

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
	if([data length] > 0) {
		[mData appendData:data];
		[mDelegate stream:(NSStream *)self handleEvent:NSStreamEventHasBytesAvailable];
	}
}

-(void) appendData:(const void*)buffer numBytes:(unsigned)numbytes
{
	[self appendData:[NSData dataWithBytes:buffer length:numbytes]];
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
	
	if(![self hasBytesAvailable]) r = NSStreamStatusAtEnd;
	
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

- (BOOL)setProperty:(id)property forKey:(NSString *)key
{
	return NO;
}


// Implement methods required by subclasses of NSInputStream
- (int)read:(uint8_t *)buffer maxLength:(unsigned int)len
{
	int bytesLeft = [mData length] - mPosition;
	int bytesToRead = MIN(bytesLeft, len);
	
	[mData getBytes:buffer range:NSMakeRange(mPosition, bytesToRead)];
	
	mPosition += bytesToRead;
	NSAssert1(mPosition <= [mData length], @"Position (%d) past valid range.", mPosition);
	return bytesToRead;
}

- (BOOL)getBuffer:(uint8_t **)buffer length:(unsigned int *)len
{
	return NO;
}

- (BOOL)hasBytesAvailable
{
	return [mData length] > mPosition;
}

@end
