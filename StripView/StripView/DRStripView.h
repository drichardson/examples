//
//  DRStripView.h
//  StripView
//
//  Created by Douglas Richardson on 4/29/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import <UIKit/UIKit.h>

@protocol DRStripViewDataSource;
@protocol DRStripViewDelegate;

@interface DRStripView : UIView

@property (nonatomic, weak) id <DRStripViewDataSource> dataSource;
@property (nonatomic, weak) id <DRStripViewDelegate> delegate;
@property (nonatomic) CGFloat columnWidth;

- (void)reload;

@end

@protocol DRStripViewDataSource <NSObject>
// The view returned will be set to the height of the strip view and the width returned by the delegate.
- (UIView*)stripView:(DRStripView*)tableView viewForColumnAtIndexPath:(NSIndexPath*)indexPath;
- (NSInteger)stripView:(DRStripView*)stripView numberOfColumnsInSection:(NSInteger)section;
@end

@protocol DRStripViewDelegate <NSObject>

@optional
// There are performance implications to using stripView:widthForColumnAtIndexPath: instead of rowHeight. Every time a table view is displayed, it calls stripView:widthForColumnAtIndexPath: on the delegate for each of its rows, which can result in a significant performance problem with table views having a large number of rows (approximately 1000 or more).

- (CGFloat)stripView:(DRStripView*)stripView widthForColumnAtIndexPath:(NSIndexPath*)indexPath;

@end
