//
//  ScrollTestAppDelegate.m
//  ScrollTest
//
//  Created by Doug Richardson on 2/21/10.
//  Copyright Doug Richardson 2010. All rights reserved.
//

#import "ScrollTestAppDelegate.h"
#import "WrappingLayoutView.h"

@implementation ScrollTestAppDelegate

@synthesize window;

static float myrand(void)
{
	return ((double)rand())/((double)INT_MAX);
}


- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {    
	
    // Override point for customization after application launch
	
	UIScrollView *scrollView = [[[UIScrollView alloc] initWithFrame:window.bounds] autorelease];
	scrollView.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
	scrollView.backgroundColor = [UIColor darkGrayColor];
	[window addSubview:scrollView];
	
	WrappingLayoutView *wrap = [[[WrappingLayoutView alloc] initWithFrame:scrollView.bounds] autorelease];
	//wrap.autoresizingMask = UIViewAutoresizingFlexibleWidth;
	wrap.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
	wrap.backgroundColor = [UIColor blueColor];
	wrap.verticalPadding = wrap.horizontalPadding = 4.0;
	[scrollView addSubview:wrap];
	
	CGRect frame = CGRectMake(0, 0, 100, 100);
	
	for (int i = 0; i < 100; ++i)
	{
		UIView *view = [[[UIView alloc] initWithFrame:frame] autorelease];
		view.backgroundColor = [UIColor colorWithRed:myrand() green:myrand() blue:myrand() alpha:1];
		[wrap addSubview:view];
		
		CGPoint center = view.center;
		center.x = scrollView.center.x;
		view.center = center;
		
		UITapGestureRecognizer *recognizer = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handleSingleTapFrom:)];
		[view addGestureRecognizer:recognizer];
	}
	
	[wrap layoutSubviews];
	
	
	CGFloat maxY = CGRectGetMaxY(((UIView*)[wrap.subviews lastObject]).frame);
	//CGRect f = wrap.frame;
	//f.size.height = maxY;
	//wrap.frame = f;
	scrollView.contentSize = CGSizeMake(scrollView.bounds.size.width, maxY);
	
    [window makeKeyAndVisible];
	
	return YES;
}


- (void)dealloc
{
    [window release];
    [super dealloc];
}

- (void)handleSingleTapFrom:(UIGestureRecognizer*)recognizer
{	
	NSLog(@"Tapped view: %@", recognizer.view);
}


@end
