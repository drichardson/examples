//
//  DXSearchResultTableView.m
//  SearchPrototype
//
//  Created by Doug on 9/23/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import "DXSearchResultTableView.h"


@implementation DXSearchResultTableView

- (BOOL)acceptsFirstResponder
{
	return NO;
}

- (void)highlightSelectionInClipRect:(NSRect)clipRect
{
    NSIndexSet *rowIndexes = [self selectedRowIndexes];
	NSColor *highlightColor = [NSColor selectedControlColor];
    
    if ([rowIndexes count] > 0)
    {
        NSRange rowsInClipRect = [self rowsInRect:clipRect];
        for (NSUInteger rowIndex = rowsInClipRect.location; rowIndex < NSMaxRange(rowsInClipRect); rowIndex++)
        {
            if ([rowIndexes containsIndex:rowIndex])
            {				
                NSRect rowRect = NSIntersectionRect([self rectOfRow:rowIndex], clipRect);
				
				[highlightColor set];
                NSRectFill(rowRect);
            }
        }
    }
}

@end
