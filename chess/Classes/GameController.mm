//
//  GameController.mm
//  chess
//
//  Created by Doug on 11/16/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "GameController.h"
#import "ChessView.h"

@implementation GameController

@synthesize glView;

-(id)init
{
	self = [super init];
	if(self)
	{
	}
	return self;
}

-(void)dealloc
{
	[glView release];
	[super dealloc];
}

-(void)runGame
{
	// TODO: Probably don't even need an animation loop unless the user is dragging something.
	glView.animationInterval = 1.0 / 30.0;
	[glView startAnimation];
}

@end
