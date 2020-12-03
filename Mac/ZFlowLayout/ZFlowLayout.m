#import "ZFlowLayout.h"

/**********************************************************************
 ZFlowLayout
 
 ZFlowLayoutSizing _sizing;
 NSSize _lastSize;
 int _numElements;
 unsigned int _gridMask;
 BOOL _ignoreThisLayoutPass, _alternatingRowColors;
 NSColor *_backgroundColor, *_gridColor;
 
 **********************************************************************/

@interface ZFlowLayout (Internal)

- (void) layout;
- (int) numElements;
- (void) forceLayout;

@end

@implementation ZFlowLayout

- (id)initWithFrame:(NSRect)frameRect
{
	if ((self = [super initWithFrame:frameRect]) != nil) 
	{
		_sizing = ZMakeFlowLayoutSizing( NSMakeSize( 100, 100 ), 10, 0, NO );
		_lastSize = NSMakeSize( 0, 0 );
		_numElements = 0;
		_ignoreThisLayoutPass = NO;
		_alternatingRowColors = NO;
		_backgroundColor = nil;
		_gridColor = [[NSColor colorWithDeviceWhite: 0.5 alpha: 0.4] retain];
		_gridMask = NSTableViewGridNone;
		
		
		[self setPostsFrameChangedNotifications: YES];
		
		NSNotificationCenter *nc = [NSNotificationCenter defaultCenter];
		[nc addObserver:self 
			   selector: @selector( frameSizeChanged: ) 
				   name: NSViewFrameDidChangeNotification 
				 object: nil ];
		
	}
	return self;
}

- (void) awakeFromNib 
{
	NSScrollView *sv;
	if ( sv = [self enclosingScrollView] )
	{
		/*
		 Resize self to fit scrollview
		 And set width/height resizable
		 */
		
		NSSize contentSize = [sv contentSize];
		[self setFrameSize: contentSize];		
		[self setAutoresizingMask: 
		 NSViewWidthSizable | NSViewHeightSizable];
	}   
}

- (void) setSizing: (ZFlowLayoutSizing) sizing
{
	if ( _sizing.minSize.width != sizing.minSize.width ||
		_sizing.minSize.height != sizing.minSize.height ||
		_sizing.padding != sizing.padding ||
		_sizing.spring != sizing.spring ||
		_sizing.oneColumn != sizing.oneColumn )
	{
		_sizing = sizing;
		[self forceLayout];
		[self display];
	}
}

- (ZFlowLayoutSizing) sizing
{
	return _sizing;
}

- (void) setBackgroundColor: (NSColor *) color
{
	[_backgroundColor autorelease];
	_backgroundColor = [color retain];
}

- (NSColor *) backgroundColor
{
	return _backgroundColor;
}

- (void) setUsesAlternatingRowBackgroundColors: 
(BOOL)useAlternatingRowColors
{
	_alternatingRowColors = useAlternatingRowColors;
}

- (BOOL) usesAlternatingRowBackgroundColors
{
	return _alternatingRowColors;
}

- (void) setGridStyleMask:(unsigned int)gridType
{
	_gridMask = gridType;
}

- (unsigned int) gridStyleMask
{
	return _gridMask;
}

- (void) setGridColor:(NSColor *)aColor
{
	[_gridColor autorelease];
	_gridColor = [aColor retain];
}

- (NSColor *) gridColor
{
	return _gridColor;
}


///////////////////////////////////////////////////////////////////////
// Internal

- (BOOL) isOpaque
{
	if ( !_alternatingRowColors )
	{
		if ( !_backgroundColor ) return NO;
		return YES;
	}
	
	return YES;
}

- (void)drawRect:(NSRect)rect 
{
	NSRect bounds = [self bounds];
	NSBezierPath *fill = [NSBezierPath bezierPathWithRect: bounds];
	
	if ( !_alternatingRowColors )
	{
		if (!_backgroundColor) return;
		[_backgroundColor set];
		[fill fill];
	}
	else
	{
		NSArray *colors = [NSColor controlAlternatingRowBackgroundColors];
		NSColor *color = nil;
		
		int row = 0;
		float rowHeight = _sizing.minSize.height + _sizing.padding;
		NSRect rowRect = NSMakeRect( 0, bounds.size.height - rowHeight, 
									bounds.size.width, rowHeight);
		
		while ( 1 )
		{
			color = [colors objectAtIndex: (row % [colors count])];
			NSBezierPath *fill = [NSBezierPath bezierPathWithRect: rowRect];
			
			[color set];
			[fill fill];
			
			rowRect.origin.y -= rowHeight;
			if ( rowRect.origin.y < -rowHeight ) break;
			
			row++;
		}
	}
	
	if ( _gridMask & NSTableViewSolidVerticalGridLineMask )
	{
		
	}
	
	if ( _gridMask & NSTableViewSolidHorizontalGridLineMask )
	{
		NSBezierPath *hLines = [NSBezierPath bezierPath];
		
		int row = 0;
		float rowHeight = _sizing.minSize.height + _sizing.padding, 
		y = bounds.size.height - 0.5;
		
		while ( 1 )
		{
			if ( row > 0 )
			{			
				[hLines moveToPoint: NSMakePoint( 0, y )];
				[hLines lineToPoint: NSMakePoint( bounds.size.width, y )];
			}
			
			y-= rowHeight;
			
			if ( y <= 0 ) break;
			
			row++;
		}
		
		[_gridColor set];
		[hLines stroke];
	}
}

- (void) layout
{
	NSRect bounds = [self bounds], elementRect;
	NSPoint origin;
	
	if ( bounds.size.width == _lastSize.width &&
		bounds.size.height == _lastSize.height  )
	{
		return;
	}
	
	_lastSize = bounds.size;	
	
	int i, j, k, numRows, numCols, widthAccumulator, 
	heightAccumulator, count;
	
	float widthPad, minWidth;
	int innerWidth = bounds.size.width - ( 2 * _sizing.padding );
	int innerHeight = bounds.size.height - ( 2 * _sizing.padding );
	float remainingWidth = 0;
	
	/*
	 Do one-column as an absurdly big minimum width
	 */
	minWidth = _sizing.oneColumn ? innerWidth : _sizing.minSize.width;
	if ( minWidth > innerWidth ) minWidth = innerWidth;
	
	count = [self numElements];
	
	/*
	 Determine max number of rows and columns
	 */
	
	widthAccumulator = 0;
	numCols = 0;
	while ( widthAccumulator <= (innerWidth + _sizing.padding) )
	{
		widthAccumulator += minWidth + _sizing.padding;
		numCols++;		
	}
	
	if ( numCols > 1 ) numCols--;
	
	heightAccumulator = 0;
	numRows = 0;
	while ( heightAccumulator <= (innerHeight + _sizing.padding) )
	{
		heightAccumulator += _sizing.minSize.height + _sizing.padding;
		numRows++;
	}
	
	if ( numRows > 1 ) numRows--;
	
	if ( count < numCols )
	{
		remainingWidth = (float) (innerWidth + _sizing.padding) - 
		( count * (minWidth + _sizing.padding )); 
		widthPad = remainingWidth / (float) count;
	}
	else
	{	
		remainingWidth = (float) (innerWidth + _sizing.padding) - 
		( numCols * (minWidth + _sizing.padding )); 
		widthPad = remainingWidth / (float) numCols;
	}
	
	if ( remainingWidth < 0 ) remainingWidth = 0;
	
	elementRect.size.width = minWidth;
	elementRect.size.height = _sizing.minSize.height;
	
	if ( !(_sizing.spring & ZSpringLeft) && !(_sizing.spring & ZSpringRight) )
		elementRect.size.width += widthPad;
	
	
	origin.x = _sizing.padding;
	origin.y = _sizing.padding / 2;
	
	/*
	 Set up origin for left & right springs
	 */
	
	// left spring only
	if ( (_sizing.spring & ZSpringLeft) && !(_sizing.spring & ZSpringRight))
	{
		origin.x = _sizing.padding + remainingWidth;
	}
	//right spring only
	else if ( !(_sizing.spring & ZSpringLeft) && (_sizing.spring & ZSpringRight))
	{
		origin.x = _sizing.padding;
	}
	//both left and right springs
	else if ( (_sizing.spring & ZSpringLeft) && (_sizing.spring & ZSpringRight))
	{
		origin.x = _sizing.padding + remainingWidth / 2.0;
	}
	
	/*
	 Now, do layout on each element rect. Use slightly 
	 different methods if in a scrollview or not. In
	 a scrollview, we layout all elements, and resize 
	 vertically to fit. Otherwise, drop any that
	 don't fit.
	 */
	
	NSArray *views = [self subviews];
	
	if ( ![self enclosingScrollView] )
	{
		k = 0;
		for ( i = 0; i < numRows; i++ )
		{
			for ( j = 0; j < numCols; j++ )
			{
				if ( k >= count ) break;
				
				elementRect.origin.x = origin.x + 
				( j * (elementRect.size.width + _sizing.padding) );
				elementRect.origin.y = bounds.size.height - origin.y - 
				( (i + 1) * (elementRect.size.height) );
				if ( i > 0 ) elementRect.origin.y -= ( i * _sizing.padding);
				
				NSView *view = [views objectAtIndex: k];	
				[view setFrame: elementRect];
				[view setHidden: NO];
				
				k++;
			}
			
			if ( k >= count ) break;
		}
		
		/*
		 Now, hide any element which was unable 
		 to be fitted into the layout
		 */
		
		while ( k < count )
		{
			[[views objectAtIndex: k++] setHidden: YES];
		}
	}
	else
	{
		/*
		 We're in a scrollview, need to layout differently.
		 */
		
		k = 0;
		i = 0;
		while ( k < count )
		{
			for ( j = 0; j < numCols; j++ )
			{
				if ( k >= count ) break;
				
				elementRect.origin.x = origin.x + 
				( j * (elementRect.size.width + _sizing.padding) );
				elementRect.origin.y = bounds.size.height - origin.y - 
				( (i + 1) * (elementRect.size.height) );
				if ( i > 0 ) elementRect.origin.y -= ( i * _sizing.padding);
				
				NSView *view = [views objectAtIndex: k];	
				[view setFrame: elementRect];
				[view setHidden: NO];
				
				k++;
			}
			
			i++; //bump row
		}
		
		float minHeight = ( i * ( elementRect.size.height + _sizing.padding ));
		NSSize contentSize = [[self enclosingScrollView] contentSize];
		
		/*
		 One-time ignore, since changing our size, here, would 
		 otherwise result in an infinite loop.
		 */
		_ignoreThisLayoutPass = YES;
		
		if ( minHeight > contentSize.height )
		{
			[self setFrameSize: NSMakeSize( contentSize.width, minHeight )];
		}
		else
		{
			[self setFrameSize: contentSize];
		}
		
	}
}

- (int) numElements
{
	return _numElements;
}

- (void) forceLayout
{
	_lastSize = NSMakeSize( 0, 0 );
	[self layout];
}

///////////////////////////////////////////////////////////////////////
// Override NSView

- (void) addSubview:(NSView *)aView
{
	[super addSubview: aView];
	_numElements++;
	[self forceLayout];
	[self display];
}

- (void)willRemoveSubview:(NSView *)subview
{
	[super willRemoveSubview: subview];
	_numElements--;
	if ( _numElements < 0 ) _numElements = 0;
	
	[self forceLayout];
	[self display];
}

- (void) frameSizeChanged: (NSNotification *) aNotification
{
	if ( _ignoreThisLayoutPass )
	{
		_ignoreThisLayoutPass = NO;
		return;
	}
	
	[self layout];
}


@end