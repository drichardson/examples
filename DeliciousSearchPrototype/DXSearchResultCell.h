//
//  DXSearchResultCell.h
//  ChildWindow
//
//  Created by Doug on 9/16/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface DXSearchResultCell : NSTextFieldCell {
@private
    NSImage *_favicon;
    NSString *_urlString;
	NSString *_notes;
    //BOOL iMouseDownInInfoButton;
    //BOOL iMouseHoveredInInfoButton;
    //SEL iInfoButtonAction;
}

+ (NSUInteger)defaultCellHeight;

- (NSImage *)favicon;
- (void)setFavicon:(NSImage *)newFavicon;

- (NSString *)urlString;
- (void)setURLString:(NSString *)newURLString;

- (NSString *)notes;
- (void)setNotes:(NSString *)newNotes;

//- (SEL)infoButtonAction;
//- (void)setInfoButtonAction:(SEL)action;

//- (NSRect)infoButtonRectForBounds:(NSRect)bounds;

@end
