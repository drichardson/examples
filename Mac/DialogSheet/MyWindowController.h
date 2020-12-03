/* MyWindowController */

#import <Cocoa/Cocoa.h>
#import "MySheeter.h"

@interface MyWindowController : NSObject
{
	MySheeter *mySheeter;
}
- (IBAction)showTheSheet:(id)sender;
@end
