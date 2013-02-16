//
//  SimplePlayerViewController.m
//  SimplePlayer
//
//  Created by Doug on 11/7/10.
//  Copyright 2010 Douglas Richardson. All rights reserved.
//

#import "SimplePlayerViewController.h"

@implementation SimplePlayerViewController



/*
// The designated initializer. Override to perform setup that is required before the view is loaded.
- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    if ((self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil])) {
        // Custom initialization
    }
    return self;
}
*/

/*
// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView {
}
*/


// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad {
	
	if ( !_player )
	{
		_player = [Player new];
		_player.rootLayer = self.view.layer;
		
		NSURL* url = [NSURL URLWithString:@"http://localhost/~doug/testvideo.m4v"];
		[_player playVideo:url];
	}
	
    [super viewDidLoad];
}


// Override to allow orientations other than the default portrait orientation.
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    // Return YES for supported orientations
    return YES; //(interfaceOrientation == UIInterfaceOrientationPortrait);
}

- (void)didReceiveMemoryWarning {
	// Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
	
	// Release any cached data, images, etc that aren't in use.
}

- (void)viewDidUnload {
	// Release any retained subviews of the main view.
	// e.g. self.myOutlet = nil;
}


- (void)dealloc {
    [super dealloc];
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"Ended: start animation");
    //CABasicAnimation* a = [CABasicAnimation animationWithKeyPath:@"opacity"];
    //a.fromValue = [NSNumber numberWithInt:1];
    //a.toValue = [NSNumber numberWithInt:0];
    
    CATransform3D transform = CATransform3DIdentity;
    transform = CATransform3DRotate(transform, M_PI_4, 0, 1, 0);
    
    CABasicAnimation* a = [CABasicAnimation animationWithKeyPath:@"transform"];
    a.fromValue = [NSValue valueWithCATransform3D:CATransform3DIdentity];
    a.toValue = [NSValue valueWithCATransform3D:transform];
    a.duration = 2.0;
    a.autoreverses = YES;
    a.repeatCount = HUGE_VAL;
    [_player.playerLayer addAnimation:a forKey:@"my animation"];
}

@end
