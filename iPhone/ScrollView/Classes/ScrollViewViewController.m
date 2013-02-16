//
//  ScrollViewAppDelegate.m
//  ScrollView
//
//  Created by Doug on 8/1/08.
//

#import "ScrollViewViewController.h"

const NSUInteger kBottomPadding = 10;

@interface ScrollViewViewController (private)
- (void)scrollToMakeEditingTextFieldVisible;
@end


@implementation ScrollViewViewController

- (void)viewDidLoad
{
	[super viewDidLoad];
	
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDidShowNotificationHandler:) name:UIKeyboardDidShowNotification object:nil];
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDidHideNotificationHandler:) name:UIKeyboardDidHideNotification object:nil];
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(deviceOrientationDidChangeNotificationHandler:) name:UIDeviceOrientationDidChangeNotification object:nil];
	
	scrollView.indicatorStyle = UIScrollViewIndicatorStyleWhite;
	scrollView.contentSize = CGSizeMake(self.view.bounds.size.width, bottomMostView.frame.origin.y + bottomMostView.frame.size.height + kBottomPadding);
}


- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
	return YES;
}


- (void)didReceiveMemoryWarning {
	[super didReceiveMemoryWarning]; // Releases the view if it doesn't have a superview
	// Release anything that's not essential, such as cached data
}


- (void)dealloc {
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[super dealloc];
}

- (void)keyboardDidShowNotificationHandler:(NSNotification*)notification
{
	NSLog(@"keyboardDidShowNotificationHandler");
	
	if(!isKeyboardVisible)
	{
		NSValue *bounds = [[notification userInfo] objectForKey:UIKeyboardBoundsUserInfoKey];			
		CGRect newFrame = scrollView.frame;
		newFrame.size.height = self.view.bounds.size.height - [bounds CGRectValue].size.height;
		
		scrollView.frame = newFrame;
		
		isKeyboardVisible = YES;
		
		[self scrollToMakeEditingTextFieldVisible];
	}
}

- (void)keyboardDidHideNotificationHandler:(NSNotification*)notification
{
	NSLog(@"keyboardDidHideNotificationHandler");
	
	if(isKeyboardVisible)
	{		
		CGRect newFrame = scrollView.frame;
		newFrame.size.height = self.view.frame.size.height - newFrame.origin.y;
		
		scrollView.frame = newFrame;
		
		isKeyboardVisible = NO;
	}
}


- (BOOL)textFieldShouldReturn:(UITextField *)textField
{
	[textField resignFirstResponder];
	return NO;
}

- (void)deviceOrientationDidChangeNotificationHandler:(NSNotification*)notification
{
	NSLog(@"Orientation changed.");
	[self scrollToMakeEditingTextFieldVisible];
}

- (void)scrollToMakeEditingTextFieldVisible
{
	if(lastFieldToBeginEditing.editing)
		[scrollView scrollRectToVisible:lastFieldToBeginEditing.frame animated:YES];
}

- (void)textFieldDidBeginEditing:(UITextField *)textField
{
	lastFieldToBeginEditing = textField;
}

@end
