//
//  DRFlowLayoutView.h
//  DRFlowLayoutView
//
//  Created by Doug Richardson on 1/20/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface DRFlowLayoutView : NSView {
	float horizontalPadding;
	float verticalPadding;
}

-(void)removeAllSubviews;

-(void)setHorizontalPadding:(float)padding;
-(void)setVerticalPadding:(float)padding;

@end
