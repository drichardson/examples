//
//  VNCWindowController.h
//  VNCClient
//
//  Created by Douglas Richardson on 3/30/07.
//  Copyright 2007 Douglas Richardson. All rights reserved.

#import <Cocoa/Cocoa.h>
#import "VNCViewWindow.h"
#import "NewConnectionWindow.h"

@interface VNCWindowController : NSObject
{
    IBOutlet NewConnectionWindow *newConnectionWindow;
    IBOutlet VNCViewWindow *vncViewWindow;
	
	BOOL mWasConnected;
}
- (IBAction)closeVNCViewWindow:(id)sender;
- (IBAction)showNewConnectionWindow:(id)sender;
- (IBAction)showVNCViewWindow:(id)sender;

-(void)connectionComplete;
-(void)newConnectionCancelled;
@end
