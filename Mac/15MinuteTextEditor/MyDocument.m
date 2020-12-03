//
//  MyDocument.m
//  15MinuteTextEditor
//
//  Created by Douglas Richardson on 3/11/07.
//  Copyright __MyCompanyName__ 2007 . All rights reserved.
//

#import "MyDocument.h"

@implementation MyDocument

+ (void) initialize {
	// Don't called parent class initializer - the runtime system does that automatically.
	[MyDocument setKeys:
		[NSArray arrayWithObjects:@"updateManager.canUndo", nil]
		triggerChangeNotificationsForDependentKey:@"updateManager.canUndo"];
}

- (id)init
{
    self = [super init];
    if (self) {
    
        // Add your subclass-specific initialization here.
        // If an error occurs here, send a [self release] message and return nil.
		
		if(mString == nil) {
			mString = [[NSAttributedString alloc] initWithString:@""];
		}
    }
    return self;
}

- (NSString *)windowNibName
{
    // Override returning the nib file name of the document
    // If you need to use a subclass of NSWindowController or if your document supports multiple NSWindowControllers, you should remove this method and override -makeWindowControllers instead.
    return @"MyDocument";
}

- (void)windowControllerDidLoadNib:(NSWindowController *) aController
{
    [super windowControllerDidLoadNib:aController];
    // Add any code here that needs to be executed once the windowController has loaded the document's window.
	
	if ([self string] != nil) {
		[[textView textStorage] setAttributedString: [self string]];
	}
}

- (NSData *)dataRepresentationOfType:(NSString *)aType
{
    // Insert code here to write your document from the given data.  You can also choose to override -fileWrapperRepresentationOfType: or -writeToFile:ofType: instead.
    
    // For applications targeted for Tiger or later systems, you should use the new Tiger API -dataOfType:error:.  In this case you can also choose to override -writeToURL:ofType:error:, -fileWrapperOfType:error:, or -writeToURL:ofType:forSaveOperation:originalContentsURL:error: instead.

	NSData *data;
	[self setString:[textView textStorage]];
	data = [NSArchiver archivedDataWithRootObject:[self string]];
	return data;
}

- (BOOL)loadDataRepresentation:(NSData *)data ofType:(NSString *)aType
{
    // Insert code here to read your document from the given data.  You can also choose to override -loadFileWrapperRepresentation:ofType: or -readFromFile:ofType: instead.
    
    // For applications targeted for Tiger or later systems, you should use the new Tiger API readFromData:ofType:error:.  In this case you can also choose to override -readFromURL:ofType:error: or -readFromFileWrapper:ofType:error: instead.
    
	NSAttributedString *tempString = [NSUnarchiver unarchiveObjectWithData: data];
	[self setString:tempString];
	
    return YES;
}

- (NSAttributedString *) string { return [[mString retain] autorelease]; }

- (void) setString: (NSAttributedString *) newValue {
	if (mString != newValue) {
		if (mString) [mString release];
		mString = [newValue copy];
	}
}

// Implement the text view's delegate method to synchronize the
// text string in the document's data model (the mString instance variable) with the
// text storage belonging to the text view, whever the user changes the text.
- (void) textDidChange: (NSNotification *)notification
{
	puts("Text changed - delegate called");
	[self setString: [textView textStorage]];
}

- (IBAction) insertDateAction:(id) sender
{	
	// Get the current date.
	NSDate *now = [NSDate dateWithTimeIntervalSinceNow:0];
	
	NSRange endRange;
	NSString *myText = [[now descriptionWithLocale:[NSLocale currentLocale]] stringByAppendingString:@"\n"];
	
	endRange.location = [[textView textStorage] length];
	endRange.length = 0;
	[textView replaceCharactersInRange:endRange withString:myText];
	endRange.length = [myText length];
	[textView scrollRangeToVisible:endRange];
}

- (IBAction) showAlertWindow:(id) sender
{
	NSAlert *alert = [NSAlert alertWithMessageText:@"This is a test alert"
									 defaultButton:nil
								   alternateButton:@"Alt Button"
									   otherButton:@"Other Button"
						 informativeTextWithFormat:@"Informative text with format placeholder"];
	
	printf("Alert return value is %d\n", [alert runModal]);
}

@end
