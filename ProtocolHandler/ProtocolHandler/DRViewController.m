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

@interface DRViewController () <NSURLConnectionDataDelegate>
@end

@implementation DRViewController
{
    AVQueuePlayer* _player;
    DRPlayerView* _playerView;
    NSURLConnection* _connection;
    NSData* _data;
}

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        _player = [[AVQueuePlayer alloc] initWithItems:@[]];
    }
    return self;
}

- (void)loadView
{
    NSLog(@"Loading view");
    _playerView = [[DRPlayerView alloc] init];
    [_playerView setPlayer:_player];
    [self setView:_playerView];
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    NSLog(@"View did load");
#if 1
    [_connection cancel];
    _connection = [NSURLConnection connectionWithRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"x://blashblash"]] delegate:self];
#else
#if 1
    AVURLAsset* asset = [AVURLAsset assetWithURL:[NSURL URLWithString:@"x://blahblahblah.m4v"]];
#else
    AVURLAsset* asset = [AVURLAsset assetWithURL:[[NSBundle mainBundle] URLForResource:@"sample_iPod" withExtension:@".m4v"]];
#endif
    AVPlayerItem* item = [AVPlayerItem playerItemWithAsset:asset];
    [_player insertItem:item afterItem:nil];
    [_player setRate:1];
#endif
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response
{
    NSLog(@"Got response: %@. Headers: %@", [response URL], [(NSHTTPURLResponse*)response allHeaderFields]);
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data
{
    NSLog(@"Receieved %d bytes of data", [data length]);
    if ( _data == nil ) _data = data;
    else assert(0);
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection
{
    NSLog(@"Connection finished");
    _connection = nil;
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error
{
    NSLog(@"COnnection failed with: %@", error);
}

@end
