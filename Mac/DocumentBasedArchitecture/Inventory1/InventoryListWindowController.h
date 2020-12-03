//
//  InventoryListWindowController.h
//  Inventory1
//
//  Created by Doug on 7/23/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface InventoryListWindowController : NSWindowController {
	IBOutlet NSTableView* tableView;
}

-(void)update;

@end
