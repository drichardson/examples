//
//  DRPlayerView.m
//  ProtocolHandler
//
//  Created by Douglas Richardson on 7/9/13.
//  Copyright (c) 2013 upthere. All rights reserved.
//

#import "DRPlayerView.h"

@implementation DRPlayerView

+ (Class)layerClass
{
    return [AVPlayerLayer class];
}

- (void)setPlayer:(AVPlayer *)player
{
    [(AVPlayerLayer*)[self layer] setPlayer:player];
}

@end
