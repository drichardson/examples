//
//  InjectableInputStream.h
//  VNCClient
//
//  Created by Douglas Richardson on 3/22/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface InjectableInputStream : NSInputStream {
	NSMutableData *mData;
	int mPosition;
	id mDelegate;
}

-(void) appendData:(NSData*)data;
-(void) appendData:(const void*)buffer numBytes:(unsigned)numbytes;

@end
