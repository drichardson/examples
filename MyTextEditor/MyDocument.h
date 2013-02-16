//
//  MyDocument.h
//  MyTextEditor
//
//  Created by Douglas Richardson on 3/12/07.
//  Copyright __MyCompanyName__ 2007 . All rights reserved.
//


#import <Cocoa/Cocoa.h>

@interface MyDocument : NSDocument
{
	IBOutlet NSTextView *textView;
	IBOutlet NSTextField *statusTextField;
	NSAttributedString *mString;
}

-(NSAttributedString*) string;
-(void) setString:(NSAttributedString*)newString;
@end
