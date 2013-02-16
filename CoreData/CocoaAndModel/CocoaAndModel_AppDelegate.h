//
//  CocoaAndModel_AppDelegate.h
//  CocoaAndModel
//
//  Created by Doug on 7/21/09.
//  Copyright Douglas Richardson 2009 . All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface CocoaAndModel_AppDelegate : NSObject 
{
    IBOutlet NSWindow *window;
    
    NSPersistentStoreCoordinator *persistentStoreCoordinator;
    NSManagedObjectModel *managedObjectModel;
    NSManagedObjectContext *managedObjectContext;
}

- (NSPersistentStoreCoordinator *)persistentStoreCoordinator;
- (NSManagedObjectModel *)managedObjectModel;
- (NSManagedObjectContext *)managedObjectContext;

- (IBAction)saveAction:sender;

@end
