//
//  TestAppDelegate.m
//  Test
//
//  Created by Doug on 4/11/10.
//  Copyright Douglas Richardson 2010. All rights reserved.
//

#import "TestAppDelegate.h"
#import <QuartzCore/QuartzCore.h>

@implementation TestAppDelegate

@synthesize window;


#pragma mark -
#pragma mark Application lifecycle

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {    
	
    // Override point for customization after application launch
	
	UIView *view = [[UIView alloc] initWithFrame:window.bounds];
	view.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
	
	CALayer *layer = view.layer;
	layer.borderColor = [[UIColor redColor] CGColor];
	layer.borderWidth = 2.0;
	//layer.backgroundColor = [[UIColor yellowColor] CGColor];
	UIImage *image = [UIImage imageNamed:@"arrow.png"];
	layer.contents = [image CGImage];
	
	layer = window.layer;
	layer.borderColor = [[UIColor greenColor] CGColor];
	layer.borderWidth = 1.0;
	layer.backgroundColor = [[UIColor grayColor] CGColor];
	
	window.frame = [UIScreen mainScreen].applicationFrame;
	
	[window addSubview:view];
	
    [window makeKeyAndVisible];
	
	[[UIDevice currentDevice] beginGeneratingDeviceOrientationNotifications];
	
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(orientationChange:) name:UIDeviceOrientationDidChangeNotification object:nil];
    
    return YES;
}


/**
 applicationWillTerminate: saves changes in the application's managed object context before the application terminates.
 */
- (void)applicationWillTerminate:(UIApplication *)application {
	
    NSError *error = nil;
    if (managedObjectContext != nil) {
        if ([managedObjectContext hasChanges] && ![managedObjectContext save:&error]) {
			/*
			 Replace this implementation with code to handle the error appropriately.
			 
			 abort() causes the application to generate a crash log and terminate. You should not use this function in a shipping application, although it may be useful during development. If it is not possible to recover from the error, display an alert panel that instructs the user to quit the application by pressing the Home button.
			 */
			NSLog(@"Unresolved error %@, %@", error, [error userInfo]);
			abort();
        } 
    }
}

- (void)orientationChange:(NSNotification*)notification
{
	NSLog(@"Orientation changed to %d", [UIDevice currentDevice].orientation);	
	
	[UIView beginAnimations:@"orientationAnimation" context:nil];
	
	CGRect bounds = window.bounds;
	CGSize size = [UIScreen mainScreen].applicationFrame.size;
	
	switch ([UIDevice currentDevice].orientation)
	{
		case UIDeviceOrientationPortrait:
			window.transform = CGAffineTransformIdentity;
			bounds.size = CGSizeMake(size.width, size.height);
			break;
			
		case UIDeviceOrientationPortraitUpsideDown:
			window.transform = CGAffineTransformMakeRotation(M_PI);
			bounds.size = CGSizeMake(size.width, size.height);
			break;
			
		case UIDeviceOrientationLandscapeLeft:
			window.transform = CGAffineTransformMakeRotation(M_PI_2);
			bounds.size = CGSizeMake(size.height, size.width);
			break;
			
		case UIDeviceOrientationLandscapeRight:
			window.transform = CGAffineTransformMakeRotation(-M_PI_2);
			bounds.size = CGSizeMake(size.height, size.width);
			break;
			
		case UIDeviceOrientationFaceUp:
		case UIDeviceOrientationFaceDown:
		case UIDeviceOrientationUnknown:
		default:
			NSLog(@"Orientation %d not handled", [UIDevice currentDevice].orientation);
			break;
	}
	
	window.bounds = bounds;
	
	[UIView commitAnimations];
	
	[[UIApplication sharedApplication] setStatusBarOrientation:[UIDevice currentDevice].orientation animated:YES];
	
}


#pragma mark -
#pragma mark Core Data stack

/**
 Returns the managed object context for the application.
 If the context doesn't already exist, it is created and bound to the persistent store coordinator for the application.
 */
- (NSManagedObjectContext *) managedObjectContext {
	
    if (managedObjectContext != nil) {
        return managedObjectContext;
    }
	
    NSPersistentStoreCoordinator *coordinator = [self persistentStoreCoordinator];
    if (coordinator != nil) {
        managedObjectContext = [[NSManagedObjectContext alloc] init];
        [managedObjectContext setPersistentStoreCoordinator: coordinator];
    }
    return managedObjectContext;
}


/**
 Returns the managed object model for the application.
 If the model doesn't already exist, it is created by merging all of the models found in the application bundle.
 */
- (NSManagedObjectModel *)managedObjectModel {
	
    if (managedObjectModel != nil) {
        return managedObjectModel;
    }
    managedObjectModel = [[NSManagedObjectModel mergedModelFromBundles:nil] retain];    
    return managedObjectModel;
}


/**
 Returns the persistent store coordinator for the application.
 If the coordinator doesn't already exist, it is created and the application's store added to it.
 */
- (NSPersistentStoreCoordinator *)persistentStoreCoordinator {
	
    if (persistentStoreCoordinator != nil) {
        return persistentStoreCoordinator;
    }
	
    NSURL *storeUrl = [NSURL fileURLWithPath: [[self applicationDocumentsDirectory] stringByAppendingPathComponent: @"Test.sqlite"]];
	
	NSError *error = nil;
    persistentStoreCoordinator = [[NSPersistentStoreCoordinator alloc] initWithManagedObjectModel:[self managedObjectModel]];
    if (![persistentStoreCoordinator addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:storeUrl options:nil error:&error]) {
		/*
		 Replace this implementation with code to handle the error appropriately.
		 
		 abort() causes the application to generate a crash log and terminate. You should not use this function in a shipping application, although it may be useful during development. If it is not possible to recover from the error, display an alert panel that instructs the user to quit the application by pressing the Home button.
		 
		 Typical reasons for an error here include:
		 * The persistent store is not accessible
		 * The schema for the persistent store is incompatible with current managed object model
		 Check the error message to determine what the actual problem was.
		 */
		NSLog(@"Unresolved error %@, %@", error, [error userInfo]);
		abort();
    }    
	
    return persistentStoreCoordinator;
}


#pragma mark -
#pragma mark Application's Documents directory

/**
 Returns the path to the application's Documents directory.
 */
- (NSString *)applicationDocumentsDirectory {
	return [NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) lastObject];
}


#pragma mark -
#pragma mark Memory management

- (void)dealloc {
	
    [managedObjectContext release];
    [managedObjectModel release];
    [persistentStoreCoordinator release];
    
	[window release];
	[super dealloc];
}


@end

