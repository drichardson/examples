#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>

int main (int argc, const char * argv[])
{
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	NSManagedObjectContext *moc = [[NSManagedObjectContext alloc] init];
	
	NSURL *momURL = [NSURL fileURLWithPath:@"./MyDataModel.mom"];
	NSManagedObjectModel *mom = [[NSManagedObjectModel alloc] initWithContentsOfURL:momURL];
	
	NSPersistentStoreCoordinator *coordinator = [[NSPersistentStoreCoordinator alloc] initWithManagedObjectModel:mom];
	[moc setPersistentStoreCoordinator:coordinator];
	
	NSPersistentStore *store = [coordinator addPersistentStoreWithType:NSXMLStoreType
														 configuration:nil
																   URL:[NSURL fileURLWithPath:[@"~/Desktop/mylog.stuff" stringByExpandingTildeInPath]]
															   options:nil
																 error:nil];
	
	if(store == nil)
	{
		fprintf(stderr, "Error creating store.\n");
		exit(1);
	}
	
	
	NSEntityDescription *runInfoEntity = [[mom entitiesByName] objectForKey:@"RunInfo"];
	NSManagedObject *runInfo = [[NSManagedObject alloc] initWithEntity:runInfoEntity insertIntoManagedObjectContext:moc];
	
	[runInfo setValue:[NSDate date] forKey:@"time"];
	[runInfo setValue:@"Testing 1, 2, 3" forKey:@"title"];
	
	if(![moc save:nil])
	{
		fprintf(stderr, "Error saving.");
		exit(1);
	}
	
	
	NSFetchRequest *request = [[NSFetchRequest alloc] init];
	[request setEntity:runInfoEntity];
	[request setSortDescriptors:[NSArray arrayWithObject:[[[NSSortDescriptor alloc] initWithKey:@"time" ascending:YES] autorelease]]];
	
	NSArray *array = [moc executeFetchRequest:request error:nil];
	
	for(NSManagedObject *ri in array)
	{
		NSDate *time = [ri valueForKey:@"time"];
		NSString *title = [ri valueForKey:@"title"];
		
		printf("Run Info: time = %s, title = %s\n", [[time description] UTF8String], [title UTF8String]);
	}
	
	[request release];	
	[moc release];
	[mom release];
	[coordinator release];
	
    [pool drain];
    return 0;
}
