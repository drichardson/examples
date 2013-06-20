//
//  main.m
//  WeakExample
//
//  Created by Douglas Richardson on 6/20/13.
//

#import <Foundation/Foundation.h>
#import "OldSchoolPool.h"

@interface MyObj : NSObject
@property (nonatomic, copy) NSString* name;
@end

@implementation MyObj

- (id)initWithName:(NSString*)name
{
    self = [super init];
    if ( self ) {
        _name = [name copy];
    }
    return self;
}

- (void)dealloc
{
    NSLog(@"MyObj dealloc'd: %@", _name);
}

- (NSString*)description
{
    return [NSString stringWithFormat:@"%@: %@", [super description], _name];
}

@end

void run_test(NSString* objectName)
{
    MyObj* obj1 = [[MyObj alloc] initWithName:objectName];
    
    __weak MyObj* weakObj1 = obj1;
    void (^myBlock)() = ^{
        MyObj* strongObj = weakObj1;
        if ( strongObj )
        {
            NSLog(@"Strong obj: %@", strongObj);
        }
    };
    
    double delayInSeconds = 10002.0;
    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(delayInSeconds * NSEC_PER_SEC));
    dispatch_after(popTime, dispatch_get_main_queue(), myBlock);
}

int main(int argc, const char * argv[])
{
    // Run the test in a pool using @autoreleasepool
    @autoreleasepool {
        run_test(@"In @autoreleasepool");
    }
    
    // Run the test without any autorelease pool
    // HERE IS THE LEAK. The object MyObj instance created here is never deallocated.
    run_test(@"Not in autorelease pool");
    
    // Run the test using NSAutoreleasePool
    OldSchoolPoolMake();
    run_test(@"In NSAutoreleasePool");
    OldSchoolPoolDrain();
    
    return 0;
}
