//
//  DRStripView.m
//  StripView
//
//  Created by Douglas Richardson on 4/29/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import "DRStripView.h"

@implementation DRStripView
{
    NSInteger _numberOfColumns;
}

@synthesize dataSource=_dataSource, delegate=_delegate, columnWidth=_columnWidth;

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        
    }
    return self;
}

- (NSArray*)_visibleSubviews
{
    if ( _numberOfColumns == 0 )
    {
        return [NSArray array];
    }
    
    NSMutableArray* result = [NSMutableArray array];
    
    BOOL visibleRangeFilled = NO;
    CGRect bounds = self.bounds;
    CGFloat startingX = CGRectGetMinX(bounds);
    CGFloat endingX = CGRectGetMaxX(bounds);
    
    if ( [_delegate respondsToSelector:@selector(stripView:widthForColumnAtIndexPath:)] )
    {
        // TODO:DOUG Implement variable width.
        NSLog(@"Variable width's not implemented");
    }
    else
    {
        const CGFloat totalWidth = _columnWidth * _numberOfColumns;
        
        for(CGFloat currentX = startingX; currentX < endingX; currentX += _columnWidth)
        {
            NSInteger index = _numberOfColumns * (currentX / totalWidth );
            assert(index < _numberOfColumns);
            UIView* cell = [_dataSource stripView:self viewForColumnAtIndexPath:[NSIndexPath indexPathForRow:index inSection:0]];
            [result addObject:cell];
        }
    }
    
    return result;
}

- (void)reload
{
    _numberOfColumns = [_dataSource stripView:self numberOfColumnsInSection:0];
    
    [self.subviews makeObjectsPerformSelector:@selector(removeFromSuperview)];
    [[self _visibleSubviews] enumerateObjectsUsingBlock:^(UIView* view, NSUInteger idx, BOOL *stop) {
        [self addSubview:view];
    }];
}

- (void)layoutSubviews
{
    // TODO:DOUG Handle variable width.
    CGRect bounds = self.bounds;
    CGSize cellSize = CGSizeMake(_columnWidth, CGRectGetHeight(bounds));
    CGFloat currentX = CGRectGetMinX(bounds);
    
    // Find the index of the first and last visible views. The left most view is aligned to the left
    // margin. The otherviews flow to the right.
    for(UIView* view in self.subviews)
    {        
        view.frame = CGRectMake(currentX, 0, cellSize.width, cellSize.height);
        currentX += cellSize.width;
    }
}

@end
