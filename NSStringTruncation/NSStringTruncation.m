#import <Foundation/Foundation.h>

@interface NSString (DXTruncationAddon)
-(NSString*)stringTruncatedInMiddleIfMaxCharactersExceeds:(unsigned)maxCharacters;
@end

@implementation NSString (DXTruncationAddon)
-(NSString*)stringTruncatedInMiddleIfMaxCharactersExceeds:(unsigned)maxCharacters
{
	unsigned len = [self length];
	NSString *result = self;
	
	if(len > maxCharacters)
	{
		// Needs to be truncated. This is the max characters plus 1 for the ellipses.
		unsigned charactersToRemove = len - maxCharacters + 1;
		unsigned characterToRemoveLeftOfMiddle = charactersToRemove / 2;
		unsigned characterToRemoveRightOfMiddle = charactersToRemove - characterToRemoveLeftOfMiddle;
		unsigned middle = len / 2;
		unsigned endOfFirstSubstring = middle - characterToRemoveLeftOfMiddle;
		unsigned beginningOfLastSubstring = middle + characterToRemoveRightOfMiddle;
		
		NSString *s1 = [self substringToIndex:endOfFirstSubstring];
		NSString *s2 = [self substringFromIndex:beginningOfLastSubstring];
		
		unichar ellipseCode = 0x2026;
		NSString *ellipse = [NSString stringWithCharacters:&ellipseCode length:1];
		result = [NSString stringWithFormat:@"%@%@%@", s1, ellipse, s2];
	}
	
	return result;
}
@end

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	NSLog(@"String truncation: %@", [@"This is a test" stringTruncatedInMiddleIfMaxCharactersExceeds:2]);
	NSLog(@"String truncation: %@", [@"This is a test" stringTruncatedInMiddleIfMaxCharactersExceeds:3]);
	NSLog(@"String truncation: %@", [@"This is a test" stringTruncatedInMiddleIfMaxCharactersExceeds:4]);
	NSLog(@"String truncation: %@", [@"This is a test" stringTruncatedInMiddleIfMaxCharactersExceeds:5]);
	
	NSLog(@"S: %@", [@"This is a lot of text that I like to type but really need to make shorter." stringTruncatedInMiddleIfMaxCharactersExceeds:10]);
    
    [pool drain];
    return 0;
}
