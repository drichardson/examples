//
//  ScrollViewViewController.h
//  ScrollView
//
//  Created by Doug on 8/1/08.
//

#import <UIKit/UIKit.h>

@interface ScrollViewViewController : UIViewController {
	IBOutlet UIScrollView *scrollView;
	IBOutlet UIView *bottomMostView; // For determining the content size for the scroll view.
	
	UITextField *lastFieldToBeginEditing;
	
	BOOL isKeyboardVisible;
}

@end

