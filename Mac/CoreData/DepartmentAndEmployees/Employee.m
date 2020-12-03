// 
//  Employee.m
//  DepartmentAndEmployees
//
//  Created by Doug Richardson on 8/26/09.
//  Copyright 2009 Doug Richardson. All rights reserved.
//

#import "Employee.h"


@implementation Employee 

@dynamic salary;
@dynamic employeeID;
@dynamic firstName;
@dynamic lastName;
@dynamic directReports;
@dynamic department;
@dynamic manager;

@dynamic primitiveEmployeeID;

- (NSString *)fullNameAndID {
    return [NSString stringWithFormat:@"%@, %@ (%@)",
            self.lastName, self.firstName, self.employeeID];
}

+ (NSSet *)keyPathsForValuesAffectingFullNameAndID {
    return [NSSet setWithObjects:
            @"lastName", @"firstName", @"employeeID", nil];
}

- (void)awakeFromInsert {
    static NSInteger tempID = 1;
	
    [super awakeFromInsert];
    self.primitiveEmployeeID = [NSNumber numberWithInteger:tempID++];
}
	

@end
