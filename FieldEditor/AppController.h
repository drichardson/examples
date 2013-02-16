//
//  AppController.h
//  FieldEditor
//
//  Created by Doug on 6/4/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface AppController : NSObject {
	IBOutlet NSTextField *plain;
	IBOutlet NSTextField *spellChecker;
	IBOutlet NSTextField *textView;
	
	NSTextView *spellCheckerFieldEditor;
	NSTextView *textViewFieldEditor;
}

-(IBAction)okPressed:(id)sender;
-(IBAction)cancelPressed:(id)sender;

@end
