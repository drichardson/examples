//
//  GameController.h
//  chess
//
//  Created by Doug on 11/16/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>

@class ChessView;

@interface GameController : NSObject {
	ChessView *glView;
}

@property (nonatomic, retain) IBOutlet ChessView *glView;

-(void)runGame;

@end
