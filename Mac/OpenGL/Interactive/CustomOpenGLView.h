//
//  CustomOpenGLView.h
//  CustomOpenGLView
//
//  Created by Doug Richardson on 12/29/10.
//  Copyright 2010 Doug Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface CustomOpenGLView : NSView

- (id)initWithFrame:(NSRect)frame;

@property (nonatomic) IBOutlet NSNumber* doubleBuffer;
@property (nonatomic) IBOutlet NSNumber* syncToVerticalRetrace;

@end
