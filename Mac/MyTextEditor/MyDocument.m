//
//  MyDocument.m
//  MyTextEditor
//
//  Created by Douglas Richardson on 3/12/07.
//  Copyright __MyCompanyName__ 2007 . All rights reserved.
//

#import "MyDocument.h"

@implementation MyDocument

- (id)init
{
    self = [super init];
    if (self) {
    
        // Add your subclass-specific initialization here.
        // If an error occurs here, send a [self release] message and return nil.
		mString = [[NSAttributedString alloc] initWithString:@""];    
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
	puts("Loaded nib for document.");
	
	[[textView textStorage] setAttributedString:[self string]];
}

- (NSData *)dataOfType:(NSString *)typeName error:(NSError **)outError
{
	[self setString:[textView textStorage]];
	return [NSArchiver archivedDataWithRootObject:[textView textStorage]];
}

- (BOOL)readFromData:(NSData *)data ofType:(NSString *)typeName error:(NSError **)outError
{
	printf("readFromData data: %p, ofType: %s\n", data, [typeName UTF8String]);
	[self setString:[NSUnarchiver unarchiveObjectWithData:data]];
	//NSTextStorage *tempString = [NSUnarchiver unarchiveObjectWithData:data];
	//[[textView textStorage] setAttributedString:tempString];
	
	return YES;
}

-(NSAttributedString*) string { return [[mString retain] autorelease]; }

-(void) setString:(NSAttributedString*)newString
{
	if(newString != mString) {
		if(mString) [mString release];
		mString = [newString copy];
	}
}

@end
