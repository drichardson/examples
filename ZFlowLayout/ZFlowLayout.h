#import <Cocoa/Cocoa.h>

/*
 No spring will let elements stretch to fit width
 
 Left spring will push elements to right side, 
 with no stretching
 
 Right spring will push elements to left side,
 with no stretching.
 
 Spring on left & right will squish layout into center.
 */
typedef enum _ZFlowLayoutSpring
{
	ZNoSpring = 0,
	ZSpringLeft = 1,
	ZSpringRight = 2,
	ZSpringLeftRight = 3,
} ZFlowLayoutSpring;

typedef struct _ZFlowLayoutSizing
{
	NSSize minSize;
	int padding;
	int spring;
	bool oneColumn;
} ZFlowLayoutSizing;

static inline ZFlowLayoutSizing ZMakeFlowLayoutSizing( NSSize minSize, int padding, 
													  int spring, BOOL oneColumn )
{
	ZFlowLayoutSizing sizing;
	sizing.minSize = minSize;
	sizing.padding = padding;
	sizing.spring = spring;
	sizing.oneColumn = oneColumn;
	return sizing;
}

/**********************************************************************
 ZFlowLayout
 **********************************************************************/

@interface ZFlowLayout : NSView
{
	ZFlowLayoutSizing _sizing;
	NSSize _lastSize;
	int _numElements;
	unsigned int _gridMask;
	BOOL _ignoreThisLayoutPass, _alternatingRowColors;
	NSColor *_backgroundColor, *_gridColor;
}

- (void) setSizing: (ZFlowLayoutSizing) sizing;
- (ZFlowLayoutSizing) sizing;

/*
 Draw a solid background color
 */
- (void) setBackgroundColor: (NSColor *) color;
- (NSColor *) backgroundColor;

/*
 Draw background using system alternating row colors
 */
- (void) setUsesAlternatingRowBackgroundColors:(BOOL)useAlternatingRowColors;

- (BOOL) usesAlternatingRowBackgroundColors;

- (void) setGridStyleMask:(unsigned int)gridType;
- (unsigned int) gridStyleMask;

- (void) setGridColor:(NSColor *)aColor;
- (NSColor *) gridColor;

@end