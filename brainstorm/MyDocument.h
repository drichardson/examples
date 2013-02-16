//
//  MyDocument.h
//  Brainstorm
//
//  Created by Douglas Richardson on 10/30/07.
//  Copyright __MyCompanyName__ 2007 . All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "BrainstormView.h"

@interface MyDocument : NSPersistentDocument {
	IBOutlet NSArrayController *arrayController;
	IBOutlet BrainstormView *brainstormView;
}

@end
