#import <Foundation/Foundation.h>


// The stuff in here is apparently deprecated in 10.5 because it is not Objective-C 2.0 compatible.

// ===================================================================
// Method Swizzling Junk from CocoaDev

#import <objc/objc-class.h>
void MethodSwizzle(Class aClass, SEL orig_sel, SEL alt_sel)
{
    Method orig_method = nil, alt_method = nil;
	
    // First, look for the methods
    orig_method = class_getInstanceMethod(aClass, orig_sel);
    alt_method = class_getInstanceMethod(aClass, alt_sel);
	
    // If both are found, swizzle them
    if ((orig_method != nil) && (alt_method != nil))
	{		
#if 0 // MAC_OS_X_VERSION_MIN_REQUIRED < MAC_OS_X_VERSION_10_6
		char *temp1;
        IMP temp2;
		
        temp1 = orig_method->method_types;
        orig_method->method_types = alt_method->method_types;
        alt_method->method_types = temp1;
		
        temp2 = orig_method->method_imp;
        orig_method->method_imp = alt_method->method_imp;
        alt_method->method_imp = temp2;
#else
		method_exchangeImplementations(orig_method, alt_method);
#endif
	}
}
// ===================================================================


@interface TheClassToHack : NSObject
-(void)doSomething;
@end

@implementation TheClassToHack

-(void)doSomething
{
	NSLog(@"doSomething called");
}

@end

@implementation TheClassToHack (ReplacementMethods)

-(void)doSomethingElse
{
	NSLog(@"doSomethingElse called");
	
	// After the swizzle, this calls the original method. Don't call before the swizzle or
	// you get infinite recursion.
	[self doSomethingElse];
}

@end

@implementation TheClassToHack (AnotherReplacementMethod)

-(void)doYetSomethingElse
{
	NSLog(@"doYetSomethingElse called");
	[self doYetSomethingElse];
}

@end

int main (int argc, const char * argv[])
{
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	TheClassToHack *classToHack = [[[TheClassToHack alloc] init] autorelease];
	
	NSLog(@"=========Before Swizzle=====================");
	[classToHack doSomething];
	
	MethodSwizzle([TheClassToHack class], @selector(doSomething), @selector(doSomethingElse));
	
	NSLog(@"=========After Swizzle 1=====================");
	[classToHack doSomething];
	
	MethodSwizzle([TheClassToHack class], @selector(doSomething), @selector(doYetSomethingElse));
	
	NSLog(@"=========After Swizzle 2=====================");
	[classToHack doSomething];

    [pool drain];
    return 0;
}
