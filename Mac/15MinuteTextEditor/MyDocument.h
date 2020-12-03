//
//  MyDocument.h
//  15MinuteTextEditor
//
//  Created by Douglas Richardson on 3/11/07.
//  Copyright __MyCompanyName__ 2007 . All rights reserved.
//


#import <Cocoa/Cocoa.h>

@interface MyDocument : NSDocument
{
	IBOutlet NSTextView *textView;
	NSAttributedString *mString;
}
- (NSAttributedString *) string;
- (void) setString: (NSAttributedString *) value;

// Interface Builder recognizes the IBAction return type as
// an action. After this method was created, I drop the MyDocuments.h
// file onto the Interface Builder instance window. Then I Control-Dragged
// from the Insert button to the File's Owner object and set the target
// to be this method.
- (IBAction) insertDateAction:(id) sender;
- (IBAction) showAlertWindow:(id) sender;
@end
