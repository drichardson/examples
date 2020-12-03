//
//  ReadableOutputStream.h
//  VNCClient
//
//  Created by Douglas Richardson on 3/23/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface ReadableOutputStream : NSOutputStream {
	NSMutableData *mData;
	int mPosition;
	id mDelegate;
}

-(int) readData:(void*)buffer maxBytes:(int)maxlen;
-(NSData*)readData;

@end
