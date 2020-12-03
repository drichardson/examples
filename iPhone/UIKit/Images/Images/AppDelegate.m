//
//  AppDelegate.m
//  Images
//
//  Created by Douglas Richardson on 3/27/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import "AppDelegate.h"

@implementation AppDelegate

@synthesize window = _window;

- (void)dealloc
{
    [_window release];
    [super dealloc];
}

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    self.window = [[[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]] autorelease];
    // Override point for customization after application launch.
    self.window.backgroundColor = [UIColor whiteColor];
    [self.window makeKeyAndVisible];
    
    //CGRect bounds = self.window.bounds;
    
    UIScreen* s = [UIScreen mainScreen];
    CGRect appFrame = s.applicationFrame;
    
    
    // Create an animated image.
    UIImage* image1 = [UIImage imageNamed:@"1"];
    NSArray* images = [NSArray arrayWithObjects:image1, [UIImage imageNamed:@"2"], [UIImage imageNamed:@"3"], [UIImage imageNamed:@"4"], nil];
    UIImage* animatedImage = [UIImage animatedImageWithImages:images duration:4.0];
    UIImageView* imageView = [[[UIImageView alloc] initWithImage:animatedImage] autorelease];
    imageView.frame = CGRectMake(appFrame.origin.x, appFrame.origin.y, image1.size.width, image1.size.height);
    imageView.backgroundColor = [UIColor yellowColor];
    [self.window addSubview:imageView];
    
    // Create an image that has end caps and is resizable.
    UIImage* resizableImage = [UIImage imageNamed:@"Resizable-15-1-15"];
    UIEdgeInsets insets;
    insets.top = 0;
    insets.left = 15;
    insets.bottom = 0;
    insets.right = 15;
    resizableImage = [resizableImage resizableImageWithCapInsets:insets];
    UIImageView* resizeView1 = [[[UIImageView alloc] initWithImage:resizableImage] autorelease];
    resizeView1.frame = CGRectMake(0, CGRectGetMaxY(imageView.frame), appFrame.size.width, resizableImage.size.height);
    [self.window addSubview:resizeView1];
    
    UIImageView* resizeView2 = [[[UIImageView alloc] initWithImage:resizableImage] autorelease];
    resizeView2.frame = CGRectMake(0, CGRectGetMaxY(resizeView1.frame), appFrame.size.width / 2.0, resizableImage.size.height);
    [self.window addSubview:resizeView2];
    
    return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application
{
    // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    // Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
}

- (void)applicationDidEnterBackground:(UIApplication *)application
{
    // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
    // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
}

- (void)applicationWillEnterForeground:(UIApplication *)application
{
    // Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
    // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
}

- (void)applicationWillTerminate:(UIApplication *)application
{
    // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
}

@end
