//
//  Player.m
//  SimplePlayer
//
//  Created by Doug on 11/7/10.
//  Copyright 2010 Douglas Richardson. All rights reserved.
//

#import "Player.h"

static NSString* kRootLayerBoundsContext = nil;

@interface Player ()
- (void)_setupPlayer:(AVPlayerItem*)playerItem;
- (void)_cleanupPlayer;

- (void)_playVideoPart2:(AVPlayerItem *)playerItem;
@end



@implementation Player

@synthesize playerLayer=_playerLayer;

- (void) dealloc
{
	self.rootLayer = nil;
	[_playerLayer release];
	
	[self _cleanupPlayer];
	
	[super dealloc];
}

- (void)_setupPlayer:(AVPlayerItem*)playerItem
{
	[self _cleanupPlayer];
	
	_player = [[AVPlayer alloc] initWithPlayerItem:playerItem];
	
	CMTime interval = CMTimeMake(1, 32); // 1/32th of a second
	
	_timeObserver = [_player addPeriodicTimeObserverForInterval:interval queue:dispatch_get_main_queue() usingBlock:^(CMTime time) {
		NSNumber* currentTime = [NSNumber numberWithDouble:CMTimeGetSeconds(_player.currentItem.currentTime)];
		_timeLayer.string = [NSNumberFormatter localizedStringFromNumber:currentTime numberStyle:NSNumberFormatterDecimalStyle];
	}];
}

- (void)_cleanupPlayer
{
	[_player removeTimeObserver:_timeObserver];
	[_player release];
	_player = nil;
}

- (void)setRootLayer:(CALayer *)layer
{
	if ( layer != _rootLayer )
	{
		[_rootLayer removeObserver:self forKeyPath:@"bounds"];
		[_rootLayer release];
		_rootLayer = [layer retain];
		[_rootLayer addObserver:self forKeyPath:@"bounds" options:0 context:&kRootLayerBoundsContext];
	}
}

- (CALayer*)rootLayer
{
	return _rootLayer;
}

- (void)playVideo:(NSURL*)url
{
	AVURLAsset* asset = [[[AVURLAsset alloc] initWithURL:url options:nil] autorelease];
	[asset loadValuesAsynchronouslyForKeys:[NSArray arrayWithObject:@"duration"] completionHandler:^(void) {
		NSLog(@"Got em: %@", [(NSString*)CMTimeCopyDescription(NULL, asset.duration) autorelease]);
	}];
	
	AVPlayerItem* playerItem = [AVPlayerItem playerItemWithAsset:asset];
	
	NSError* error;
	AVKeyValueStatus status = [asset statusOfValueForKey:@"tracks" error:&error];
	
	if ( status == AVKeyValueStatusLoaded )
	{
		[self _playVideoPart2:playerItem];
	}
	else
	{
		[asset loadValuesAsynchronouslyForKeys:[NSArray arrayWithObject:@"tracks"] completionHandler:^(void) {
			[[NSOperationQueue mainQueue] addOperationWithBlock:^(void) {
				[self _playVideoPart2:playerItem];
			}];
		}];
	}
}

- (void)_addVisuals
{
	_timeLayer = [CATextLayer new];
	_timeLayer.string = @"0.0";
	_timeLayer.font = @"Helvetica";
	_timeLayer.fontSize = 30.0;
	_timeLayer.frame = CGRectMake(0, 0, _playerLayer.bounds.size.width, 40);
	_timeLayer.backgroundColor = [[UIColor purpleColor] CGColor];
	_timeLayer.opacity = 0.3;
	//text.backgroundColor = [[UIColor colorWithRed:1.0 green:0 blue:1.0 alpha:0.5] CGColor];
	[_rootLayer addSublayer:_timeLayer];
	
	CABasicAnimation* a = [CABasicAnimation animationWithKeyPath:@"position"];
	a.toValue = [NSValue valueWithCGPoint:CGPointMake(_timeLayer.position.x, _playerLayer.bounds.size.height - _timeLayer.bounds.size.height)];
	a.repeatCount = HUGE_VAL;
	a.duration = 3.0;
	a.autoreverses = YES;
	[_timeLayer addAnimation:a forKey:nil];
}

- (void)_playVideoPart2:(AVPlayerItem*)playerItem
{
	[self _setupPlayer:playerItem];
	
	if ( _playerLayer == nil )
	{
		_playerLayer = [AVPlayerLayer new];
	}
	
	_playerLayer.player = _player;
	
	//_playerLayer.borderColor = [[UIColor redColor] CGColor];
	//_playerLayer.borderWidth = 2.0;
	//_playerLayer.videoGravity = AVLayerVideoGravityResize;
	_playerLayer.frame = _rootLayer.bounds;
	
	_playerLayer.backgroundColor = [[UIColor yellowColor] CGColor];
	_rootLayer.backgroundColor = [[UIColor blueColor] CGColor];
	
	[_player play];
	
	[_rootLayer addSublayer:_playerLayer];
	
	[self _addVisuals];
}

- (void) observeValueForKeyPath:(NSString *)keyPath ofObject:(id)object change:(NSDictionary *)change context:(void *)context
{
    if (context == &kRootLayerBoundsContext)
	{
		_playerLayer.frame = _rootLayer.bounds;
	}
	else
	{
		[super observeValueForKeyPath:keyPath ofObject:object change:change context:context];
	}
}


@end
