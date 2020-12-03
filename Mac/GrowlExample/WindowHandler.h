/* WindowHandler */

#import <Cocoa/Cocoa.h>

@interface WindowHandler : NSObject
{
    IBOutlet NSTextField *mName;
}
- (IBAction)postGrowlNotification:(id)sender;
@end
