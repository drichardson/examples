//
//  WalkieTalkieViewController.h
//  WalkieTalkie
//
//  Created by Doug on 1/22/09.
//  Copyright Douglas Richardson 2009. All rights reserved.
//

#import <UIKit/UIKit.h>
#include <sys/socket.h>
#include <netinet/in.h>

struct AQRecorderState;
struct AQPlaybackState;

@interface WalkieTalkieViewController : UIViewController
{
@private
	int writeSocket;
	int readSocket;
	struct sockaddr_in _saddr;
	int sequence;
	struct AQRecorderState *_aqData;
	struct AQPlaybackState *_aqPlayback;
}

- (IBAction)talkDown:(id)sender;
- (IBAction)talkUp:(id)sender;

@end

