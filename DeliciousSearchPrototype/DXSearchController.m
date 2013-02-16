//
//  DXSearchController.m
//  DeliciousSafari
//
//  Created by Doug on 9/16/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import "DXSearchController.h"
#import "DXSearchResultCell.h"


@implementation DXSearchController

- (id)init
{
	return [super initWithWindowNibName:@"DXSearch"];
}

- (void)awakeFromNib
{
	[tableView setNextResponder:searchField];
	
	[tableView setRowHeight:[DXSearchResultCell defaultCellHeight]];
	[[self window] center];
}

- (BOOL)control:(NSControl *)control textView:(NSTextView *)textView doCommandBySelector:(SEL)command
{
	NSLog(@"doCommand: %@", NSStringFromSelector(command));
	
	if(command == @selector(moveDown:) || command == @selector(moveDownAndModifySelection:) ||
	   command == @selector(moveUp:) || command == @selector(moveUpAndModifySelection:))
	{
		//[tableView performSelector:command withObject:self];
		[tableView keyDown:[NSApp currentEvent]];
		return YES;
	}
	
	return NO;
}

#pragma mark NSTableViewDataSource methods

- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView
{
	return 2;
}

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex
{
	NSString *value;
	if(rowIndex == 0)
		value = @"Wordpress Blog Platform";
	else {
		value = @"The Motley Fool - Investment Advice";
	}
	
	NSDictionary *attributes = [NSDictionary dictionaryWithObjectsAndKeys:[NSFont boldSystemFontOfSize:14.0], NSFontAttributeName, nil];

	return [[[NSAttributedString alloc] initWithString:value attributes:attributes] autorelease];
}

#pragma mark NSTableViewDelegate methods
- (void)tableView:(NSTableView *)aTableView willDisplayCell:(id)aCell forTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex
{
	DXSearchResultCell *cell = (DXSearchResultCell*)aCell;
	
	if(rowIndex == 0)
	{
		[cell setFavicon:[NSImage imageNamed:@"MyImage"]];
		[cell setURLString:@"http://wordpress.org/"];
		[cell setNotes:@"WordPress is a state-of-the-art publishing platform with a focus on aesthetics, web standards, and usability. WordPress is both free and priceless at the same time. More simply, WordPress is what you use when you want to work with your blogging software, not fight it. New to blogging? Learn more about WordPress, then follow the three easy steps below to start blogging in minutes. Or, for the ultimate in ease of use, get a free blog on WordPress.com."];
	}
	else if(rowIndex == 1)
	{
		[cell setFavicon:[NSImage imageNamed:@"MyImage2"]];
		[cell setURLString:@"http://www.fool.com/"];
		[cell setNotes:@"Founded in 1993 in Alexandria, VA., by brothers David and Tom Gardner, The Motley Fool is a multimedia financial-services company dedicated to building the world's greatest investment community. Reaching millions of people each month through its website, books, newspaper column, television appearances, and subscription newsletter services, The Motley Fool champions shareholder values and advocates tirelessly for the individual investor. The company's name was taken from Shakespeare, whose wise fools both instructed and amused, and could speak the truth to the king -- without getting their heads lopped off."];
	}
}

@end
