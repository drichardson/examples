//
//  Notifier.h
//  GrowlExample
//
//  Created by Douglas Richardson on 6/21/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Growl/Growl.h"

@interface Notifier : NSObject <GrowlApplicationBridgeDelegate> {

}

+(Notifier*)defaultNotifier;

-(void)buttonPushed:(NSString*)message;

@end
