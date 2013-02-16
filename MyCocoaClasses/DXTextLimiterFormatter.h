//
//  TextLimiterFormatter.h
//  Safari Delicious Extension
//
//  Created by Douglas Richardson on 9/16/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface DXTextLimiterFormatter : NSFormatter {
	unsigned int limit;
}

- (id)initWithLimit:(unsigned int)textLimit;

@end
