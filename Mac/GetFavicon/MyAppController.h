//
//  MyAppController.h
//  GetFavicon
//
//  Created by Doug on 4/20/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MyAppController : NSObject {
	IBOutlet NSTextField *mDomain;
	IBOutlet NSImageView *mImageView;
}

-(IBAction)getFavicon:(id)sender;

@end
