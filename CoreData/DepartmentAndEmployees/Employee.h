//
//  Employee.h
//  DepartmentAndEmployees
//
//  Created by Doug Richardson on 8/26/09.
//  Copyright 2009 Doug Richardson. All rights reserved.
//

#import <CoreData/CoreData.h>


@interface Employee :  NSManagedObject  
{
}

@property (nonatomic, retain) NSDecimalNumber * salary;
@property (nonatomic, retain) NSNumber * employeeID;
@property (nonatomic, retain) NSString * firstName;
@property (nonatomic, retain) NSString * lastName;
@property (nonatomic, retain) NSSet* directReports;
@property (nonatomic, retain) NSManagedObject * department;
@property (nonatomic, retain) NSManagedObject * manager;

@property (nonatomic, readonly) NSString *fullNameAndID;
@property (retain) NSNumber *primitiveEmployeeID;

@end


@interface Employee (CoreDataGeneratedAccessors)
- (void)addDirectReportsObject:(NSManagedObject *)value;
- (void)removeDirectReportsObject:(NSManagedObject *)value;
- (void)addDirectReports:(NSSet *)value;
- (void)removeDirectReports:(NSSet *)value;

@end

