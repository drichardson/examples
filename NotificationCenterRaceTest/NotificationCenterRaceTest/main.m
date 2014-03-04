//
//  main.m
//  NotificationCenterRaceTest
//
//  Created by Doug Richardson on 2/21/14.
//

#import <Foundation/Foundation.h>
#import <Foundation/NSDebug.h>

int notificationCount;

@interface ObservingObject : NSObject
@end

int main(int argc, const char * argv[])
{
    if (!NSZombieEnabled) {
        NSLog(@"Zombies must be enabled for this test");
        abort();
    }

    NSNotificationCenter* nc = [NSNotificationCenter defaultCenter];

    @autoreleasepool {
        ObservingObject* o = [ObservingObject new];
        [nc postNotificationName:@"theNotification" object:nil];

        dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
            sleep(1); // sleep so this guy gets to be the last release that causes o to dealloc
            NSLog(@"o is %@", o);
        });
    }

    sleep(2);
    // post a notification after the dispatch_async block above starts but before ObservingObject dealloc completes.
    [nc postNotificationName:@"theNotification" object:nil];

    sleep(10);

    NSLog(@"Well, everything is a I guess notification center doesn't have the race condition");
    abort(); // Won't get here because zombies will crash before.

    return 0;
}


@implementation ObservingObject

- (id)init
{
    NSLog(@"ObservingObject init");
    self = [super init];
    if (self) {
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(theNotification:) name:@"theNotification" object:nil];
    }
    return self;
}

- (void)dealloc
{
    NSLog(@"ObservingObject dealloc. Sleeping. If you get a notification now it's bad. Sleeping so that dealloc doesn't complete yet...");
    sleep(5);
    NSLog(@"ObservingObject dealloc. OK really going away now");
}

- (void)theNotification:(NSNotification*)notification
{
    ++notificationCount;

    if (notificationCount == 2) {
        // let the object dealloc
        sleep(6);

        // Referencing self here is going to crash.
        NSLog(@"About to crash... got the notification for %@", self);

        NSLog(@"You'll never get here because zombies will crash above");
        abort(); // shouldn't happen.
    }

    NSLog(@"Got the notification for %@", self);
}

@end
