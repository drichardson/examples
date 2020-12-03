//
//  MyDocument.m
//  Brainstorm
//
//  Created by Douglas Richardson on 10/30/07.
//  Copyright __MyCompanyName__ 2007 . All rights reserved.
//

#import "MyDocument.h"

@implementation MyDocument

- (id)init 
{
    self = [super init];
    if (self != nil) {
        // initialization code
    }
    return self;
}

- (NSString *)windowNibName 
{
    return @"MyDocument";
}

- (void)windowControllerDidLoadNib:(NSWindowController *)windowController 
{
	//NSLog(@"windowControllerDidLoadNib:");
	
    [super windowControllerDidLoadNib:windowController];
	
    // user interface preparation code
	
	[brainstormView bind:@"notes" toObject:arrayController withKeyPath:@"arrangedObjects" options:nil];
}

@end
