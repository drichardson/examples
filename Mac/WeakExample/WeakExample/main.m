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
    return _name;
}

@end

void run_test(NSString* objectName)
{
    MyObj* obj1 = [[MyObj alloc] initWithName:objectName];
    
#if 1
    
    // High level demonstration
    __weak MyObj* weakObj1 = obj1;
    NSLog(@"Use it: %p\n", weakObj1);
    
#else
    // Low level breakdown. For more details, see:
    // http://clang.llvm.org/docs/AutomaticReferenceCounting.html#arc-runtime-objc-initweak
    extern void* objc_initWeak(void**, void*);
    extern void objc_destroyWeak(void**);
    extern void* objc_loadWeak(void**);
    
    void* weakObj1 = nil;
    void* result = objc_initWeak(&weakObj1, (__bridge void *)(obj1));
    assert(result == weakObj1);
    
#if 1
    // Here is the call that requires the autorelease pool. If you comment this out, all instances of MyObj will be deallocated.
    void* myID = objc_loadWeak(&weakObj1);
#endif
    
    
    objc_destroyWeak(&weakObj1);
    assert(weakObj1 == nil);
#endif
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
