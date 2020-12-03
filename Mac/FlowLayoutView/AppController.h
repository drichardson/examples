//
//  AppController.h
//  ControlLayoutView
//
//  Created by Doug Richardson on 1/20/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "DRFlowLayoutView.h"

@interface AppController : NSObject {
	IBOutlet DRFlowLayoutView *flowLayoutView;
}

-(IBAction)addPressed:(id)sender;
-(IBAction)removeAllPressed:(id)sender;

@end
