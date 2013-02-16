//
//  AppController.h
//  Layout
//
//  Created by Doug on 1/20/08.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "ZFlowLayout.h"

@interface AppController : NSObject {
	IBOutlet ZFlowLayout* flowLayout;
}

-(IBAction)addButtonPressed:(id)sender;

@end
