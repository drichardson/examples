//
//  Player.h
//  SimplePlayer
//
//  Created by Doug on 11/7/10.
//  Copyright 2010 Douglas Richardson. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <AVFoundation/AVFoundation.h>

@interface Player : NSObject {
	CALayer* _rootLayer;
	AVPlayerLayer* _playerLayer;
	AVPlayer* _player;
	id _timeObserver;
	CATextLayer* _timeLayer;
}

@property (retain) CALayer* rootLayer;
@property (readonly) AVPlayerLayer* playerLayer;

- (void)playVideo:(NSURL*)url;

@end
