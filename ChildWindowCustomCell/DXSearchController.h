//
//  DXSearchController.h
//  DeliciousSafari
//
//  Created by Doug on 9/16/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "DXBorderlessWindow.h"

@interface DXSearchController : NSWindowController
{
	IBOutlet DXBorderlessWindow *resultWindow;
	IBOutlet NSTableView *tableView;
}

- (id)init;

@end
