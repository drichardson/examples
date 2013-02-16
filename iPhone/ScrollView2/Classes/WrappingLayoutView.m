//
//  WrappingLayoutView.m
//  Bookmarks
//
//  Created by Doug Richardson on 2/14/10.
//  Copyright 2010 Doug Richardson All rights reserved.
//

#import "WrappingLayoutView.h"

@implementation WrappingLayoutView

// Override viewsInOrder to change the default ordering.
- (NSArray*)viewsInOrder
{
	return self.subviews;
}

- (void)layoutSubviews
{	
	// Starting from the first control, draw it and maintain a width and height.
	// Keep moving to the right + padding until you can't draw a control, then wrap. Wrap
	// using the max control height in the row we just finished + padding.
	
	CGSize myBoundsSize = self.bounds.size;
	
	float currentX = 0, currentY = 0;
	float maxHeightForCurrentRow = 0;
	int objectCountForRow = 0;
	
	for(UIView* view in [self viewsInOrder])
	{
		CGRect frame = view.frame;
		
		if(objectCountForRow > 0 && currentX + frame.size.width >= myBoundsSize.width)
		{
			currentY += maxHeightForCurrentRow + verticalPadding;
			currentX = 0;
			objectCountForRow = 0;
			maxHeightForCurrentRow = 0;
		}
		else
			objectCountForRow++;
		
		frame.origin = CGPointMake(currentX, currentY);
		view.frame = frame;
		
		currentX += horizontalPadding + frame.size.width;
		if(frame.size.height > maxHeightForCurrentRow)
			maxHeightForCurrentRow = frame.size.height;
	}
}

- (CGFloat)horizontalPadding
{
	return horizontalPadding;
}

- (void)setHorizontalPadding:(CGFloat)newPadding
{
	if (newPadding != horizontalPadding)
	{
		NSString *key = @"horizontalPadding";
		[self willChangeValueForKey:key];
		horizontalPadding = newPadding;
		[self didChangeValueForKey:key];
		
		[self setNeedsLayout];
	}
}

- (CGFloat)verticalPadding
{
	return verticalPadding;
}

- (void)setVerticalPadding:(CGFloat)newPadding
{
	if (newPadding != verticalPadding)
	{
		NSString *key = @"verticalPadding";
		[self willChangeValueForKey:key];
		verticalPadding = newPadding;
		[self didChangeValueForKey:key];
		
		[self setNeedsLayout];
	}
}

@end
