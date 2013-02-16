//
//  AppController.m
//  FieldEditor
//
//  Created by Doug on 6/4/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "AppController.h"


@implementation AppController

-(void)awakeFromNib
{
	spellCheckerFieldEditor = [[NSTextView alloc] initWithFrame:NSZeroRect];
	[spellCheckerFieldEditor setFieldEditor:YES];
	[spellCheckerFieldEditor setContinuousSpellCheckingEnabled:YES];
	
	textViewFieldEditor = [[NSTextView alloc] initWithFrame:NSZeroRect];
	[textViewFieldEditor setFieldEditor:YES];
}

- (id)windowWillReturnFieldEditor:(NSWindow *)window toObject:(id)anObject
{
	if(anObject == spellChecker)
	{
		NSLog(@"Returning Spell Checker Field Editor");
		return spellCheckerFieldEditor;
	}
	else if(anObject == textView)
	{
		NSLog(@"Returning Text View Field Editor");
		return textViewFieldEditor;
	}
	
	NSLog(@"Returning default field editor");
	return nil;
}


-(IBAction)okPressed:(id)sender
{
	NSLog(@"ok pressed");
}

-(IBAction)cancelPressed:(id)sender
{
	NSLog(@"cancel pressed");
}

- (void)controlTextDidEndEditing:(NSNotification *)aNotification
{
	NSTextView *fieldEditor = [[aNotification userInfo] objectForKey:@"NSFieldEditor"];
	
	//NSLog(@"fieldEditor is %@", fieldEditor);
	
	if(fieldEditor == spellCheckerFieldEditor)
	{
		NSLog(@"Checking spelling");
		[fieldEditor checkSpelling:self];
	}
}
		 

@end
