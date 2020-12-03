//
//  AppController.h
//  Internationalization
//
//  Created by Doug on 2/19/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface AppController : NSObject {
	IBOutlet NSTextField *inputField;
	IBOutlet NSTextField *label;
}

-(IBAction)buttonPressed:(id)sender;
-(IBAction)loadLocalizedText:(id)sender;

@end
