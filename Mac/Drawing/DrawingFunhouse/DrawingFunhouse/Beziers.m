//
//  Beziers.m
//  DrawingFunhouse
//
//  Created by Douglas Richardson on 3/7/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import "Beziers.h"

@implementation Beziers

- (void)drawRect:(NSRect)dirtyRect
{
    [[NSColor whiteColor] setFill];
    [[NSColor blackColor] setStroke];
    NSRectFill(dirtyRect);
    
    NSBezierPath* path = [NSBezierPath bezierPathWithOvalInRect:NSMakeRect(10, 10, 40, 40)];
    [path setLineWidth:1];
    CGFloat pattern[3] = { 5.0, 2.0, 5.0 };
    [path setLineDash:pattern count:2 phase:0];
    [path stroke];
    
    [[NSColor yellowColor] setFill];
    path = [NSBezierPath bezierPathWithRoundedRect:NSMakeRect(60, 10, 40, 40) xRadius:10 yRadius:10];
    [path fill];
    [path stroke];
    
    path = [NSBezierPath bezierPath];
    NSPoint point[3];
    point[0].x = 110; point[0].y = 10;
    point[1].x = 130; point[1].y = 30;
    point[2].x = 150; point[2].y = 10;
    [path appendBezierPathWithPoints:point count:3];
    path.lineWidth = 10;
    [path stroke];
    
    NSAffineTransform* transform = [NSAffineTransform transform];
    [transform translateXBy:50 yBy:0];
    
    [path transformUsingAffineTransform:transform];
    path.lineJoinStyle = NSBevelLineJoinStyle;
    [path stroke];
    
    [path transformUsingAffineTransform:transform];
    path.lineJoinStyle = NSRoundLineJoinStyle;
    [path stroke];
    
    [path transformUsingAffineTransform:transform];
    path.lineCapStyle = NSSquareLineCapStyle;
    [path stroke];
    
    [path transformUsingAffineTransform:transform];
    path.lineCapStyle = NSRoundLineCapStyle;
    [path stroke];
    
    path = [NSBezierPath bezierPath];
    [path moveToPoint:NSMakePoint(0, 0)];
    NSFont* font = [NSFont systemFontOfSize:40.0];
    NSGlyph glyphs[2];
    glyphs[0] = [font glyphWithName:@"h"];
    glyphs[1] = [font glyphWithName:@"i"];
    [path appendBezierPathWithGlyphs:glyphs count:2 inFont:font];
    transform = [NSAffineTransform transform];
    [transform translateXBy:0 yBy:100];
    [path transformUsingAffineTransform:transform];
    [[NSColor blackColor] setFill];
    [[NSColor redColor] setStroke];
    [path fill];
    [path stroke];
    
    NSImage* image = [NSImage imageNamed:@"seascape"];
    NSRect dstImageRect = NSMakeRect(100, 100, image.size.width / 4.0, image.size.height / 4.0);
    path = [NSBezierPath bezierPathWithOvalInRect:dstImageRect];
    [path setClip];
    path = [NSBezierPath bezierPathWithRect:NSMakeRect(dstImageRect.origin.x, dstImageRect.origin.y, 1000, image.size.height / 8.0)];
    [path addClip];
    [image drawInRect:dstImageRect fromRect:NSMakeRect(0, 0, image.size.width, image.size.height) operation:NSCompositeSourceOver fraction:1];
}

@end
