//
//  DRPageView.h
//  notes
//
//  Created by Doug on 2/18/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface DRPageView : NSView {
	NSArray *contentItems;
}

@property(retain, readwrite) NSArray* contentItems;

@end
