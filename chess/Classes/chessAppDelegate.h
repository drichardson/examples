//
//  chessAppDelegate.h
//  chess
//
//  Created by Doug on 11/16/08.
//  Copyright Douglas Richardson 2008. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "GameController.h"

@class ChessView;

@interface chessAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
	GameController *gameController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet GameController *gameController;

@end

