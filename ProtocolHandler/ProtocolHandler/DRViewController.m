//
//  DRViewController.m
//  ProtocolHandler
//
//  Created by Douglas Richardson on 7/9/13.
//  Copyright (c) 2013 upthere. All rights reserved.
//

#import "DRViewController.h"
#import <AVFoundation/AVFoundation.h>
#import "DRPlayerView.h"

@interface DRViewController () <AVAssetResourceLoaderDelegate>
@end

@implementation DRViewController
{
    AVQueuePlayer* _player;
    DRPlayerView* _playerView;
    dispatch_queue_t _resourceLoaderQueue;
}

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        _player = [[AVQueuePlayer alloc] initWithItems:@[]];
        _resourceLoaderQueue = dispatch_queue_create("resourceLoaderQueue", 0);
    }
    return self;
}

- (void)loadView
{
    _playerView = [[DRPlayerView alloc] init];
    [_playerView setPlayer:_player];
    [self setView:_playerView];
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    AVURLAsset* asset = [AVURLAsset assetWithURL:[NSURL URLWithString:@"something://devimages.apple.com/iphone/samples/bipbop/bipbopall.m3u8"]];
    [[asset resourceLoader] setDelegate:self queue:_resourceLoaderQueue];
    AVPlayerItem* item = [AVPlayerItem playerItemWithAsset:asset];
    [_player insertItem:item afterItem:nil];
    [_player setRate:1];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark Resource Loading Delegate

- (BOOL)resourceLoader:(AVAssetResourceLoader *)resourceLoader shouldWaitForLoadingOfRequestedResource:(AVAssetResourceLoadingRequest *)loadingRequest
{
    assert(dispatch_get_current_queue() == _resourceLoaderQueue);
    
    NSLog(@"Should wait for request? %@", [[loadingRequest request] URL]);
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        NSString* path = [[NSBundle mainBundle] pathForResource:@"sample_iPod" ofType:@"m4v"];
        NSLog(@"Path is %@", path);
        NSData* data = [NSData dataWithContentsOfFile:path];
        
        NSURL* url = [[loadingRequest request] URL];
        NSString* mime = @"video/mp4";
        NSInteger contentLength = [data length];
        
        //NSURLResponse* response = [[NSURLResponse alloc] initWithURL:url MIMEType:mime expectedContentLength:contentLength textEncodingName:nil];
        NSHTTPURLResponse* response = [[NSHTTPURLResponse alloc] initWithURL:url statusCode:200 HTTPVersion:@"HTTP/1.1" headerFields:@{@"Content-Type": mime, @"Content-Length": [NSString stringWithFormat:@"%d", contentLength]}];
        
        [loadingRequest finishLoadingWithResponse:response data:data redirect:nil];
    });
    
    return YES;
}

@end
