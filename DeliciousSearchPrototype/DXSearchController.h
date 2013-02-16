//
//  DXSearchController.h
//  DeliciousSafari
//
//  Created by Doug on 9/16/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface DXSearchController : NSWindowController
{
	IBOutlet NSTableView *tableView;
	IBOutlet NSSearchField *searchField;
}

- (id)init;

@end
