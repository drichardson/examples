// Defines protocols for comparable objects.

#import <objc/objc.h>

typedef enum ComparisonResultEnum {
	CRGreaterThan,
	CREqualTo,
	CRLessThan
} ComparisonResult;

@protocol OrderedComparable
-(ComparisonResult) compare:(id)obj;
@end
