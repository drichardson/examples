//
//  Run.h
//  CDCLI
//
//  Created by Doug on 7/20/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import <CoreData/CoreData.h>


@interface Run : NSManagedObject
{
	NSInteger processID;
}

@property (retain) NSDate *date;
@property (retain) NSDate *primitiveDate;
@property NSInteger processID;

@end
