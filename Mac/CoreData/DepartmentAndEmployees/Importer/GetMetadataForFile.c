//
//  GetMetadataForFile.c
//  DepartmentAndEmployees Spotlight Importer
//
//  Created by Doug Richardson on 8/25/09.
//  Copyright (c) 2009 Doug Richardson. All rights reserved.
//

#include <CoreFoundation/CoreFoundation.h>
#import <CoreData/CoreData.h>


//==============================================================================
//
//	Get metadata attributes from document files
//
//	The purpose of this function is to extract useful information from the
//	file formats for your document, and set the values into the attribute
//  dictionary for Spotlight to include.
//
//==============================================================================


Boolean GetMetadataForFile(void* thisInterface, 
			   CFMutableDictionaryRef attributes, 
			   CFStringRef contentTypeUTI,
			   CFStringRef pathToFile)
{
    /* Pull any available metadata from the file at the specified path */
    /* Return the attribute keys and attribute values in the dict */
    /* Return TRUE if successful, FALSE if there was no data provided */

    NSAutoreleasePool *pool  = [[NSAutoreleasePool alloc] init];
    NSError *error = nil;
    Boolean ok = FALSE;
    
    // Create the URL, then attempt to get the meta-data from the store
    NSURL *url = [NSURL fileURLWithPath: (NSString *)pathToFile];
    NSDictionary *metadata = [NSPersistentStoreCoordinator 
        metadataForPersistentStoreOfType:nil URL:url error:&error];

    // If there is no error, add the info
    if ( error == NULL ) {

		// Get the information you are interested in from the dictionary
        // "YOUR_INFO" should be replaced by key(s) you are interested in
		NSObject *contentToIndex = [metadata objectForKey: @"YOUR_INFO"];
		if ( contentToIndex != nil ) {
		
			// Add the metadata to the text content for indexing
			[(NSMutableDictionary *)attributes setObject:contentToIndex 
				forKey:(NSString *)kMDItemTextContent];
			ok = TRUE;    
		}
    }
	
	// Return the status
    [pool drain]; pool = nil;
    return ok;
}
