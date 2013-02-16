/* MySheeter */

#import <Cocoa/Cocoa.h>

@interface MySheeter : NSObject
{
    IBOutlet NSWindow *sheetWindow;
}
- (IBAction)cancelPressed:(id)sender;
- (IBAction)okPressed:(id)sender;
-(NSWindow*)mySheetWindow;
@end
