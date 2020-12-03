#import <Foundation/Foundation.h>
#import <sqlite3.h>
#include <CommonCrypto/CommonDigest.h>

static const unsigned int kTestDataCount = 2000;
static NSArray *testDataArray;

static NSMutableArray *buildTestData();
static void sqliteTest();
static void plistTest();
static void fileSystemTest();

static void startTimer();
static void stopTimer(NSString* message);

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	
	testDataArray = buildTestData();
	
	NSLog(@"Starting File System Test");
	fileSystemTest();
	
	NSLog(@"Starting SQLite Test");
	sqliteTest();
	
	NSLog(@"Starting PList Test");
	plistTest();
	
	NSLog(@"TESTS COMPLETE");
    [pool drain];
    return 0;
}

static NSMutableArray* buildTestData()
{
	// Build an array using random data so that none of the schemes in test can get an advantage using redundant data.
	
	NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	
	FILE *fp_random = fopen("/dev/random", "r");
	
	// Build the test data.
	NSMutableArray *result = [[NSMutableArray array] retain];
	for(int i = 0; i < kTestDataCount; ++i)
	{
		unsigned char buffer[1000];
		unsigned size = 500 + (i % 50);
		size_t count = fread(buffer, 1, size, fp_random);
		assert(count == size);
		
		NSData *data = [NSData dataWithBytes:buffer length:size];
		assert(data);
		
		[result addObject:data];
	}
	
	fclose(fp_random);
	
	[pool drain];
	return [result autorelease];
}

static sqlite3* initializeDatabase()
{
	NSString* databaseFilename = @"/tmp/DataStoreComparison-sqlite.db";
	
	if([[NSFileManager defaultManager] fileExistsAtPath:databaseFilename])
	{
		BOOL result = [[NSFileManager defaultManager] removeItemAtPath:databaseFilename error:nil];
		assert(result);
	}
	
	sqlite3* pDB = NULL;
	int result = sqlite3_open([databaseFilename UTF8String], &pDB);
	assert(result == SQLITE_OK);
	
	char *errorMessage = NULL;
	result = sqlite3_exec(pDB, "create table images (id integer primary key autoincrement, faviconURL text not null, data blob not null)", NULL, NULL, &errorMessage);
	if(result != SQLITE_OK)
	{
		NSLog(@"Error creating image table in SQLite database. %s", errorMessage);
		sqlite3_free(errorMessage);
		exit(1);
	}
	
	return pDB;
}

static void sqliteTest()
{
	NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	
	sqlite3 *pDB = initializeDatabase();
	sqlite3_stmt *pInsertStatement = NULL;
	
	startTimer();
	
	int result = sqlite3_prepare_v2(pDB, "insert into images (faviconURL, data) values (?, ?)", -1, &pInsertStatement, NULL);
	assert(result == SQLITE_OK);
	int i = 0;
	
	stopTimer(@"Preparing SQL statement");
	
	startTimer();
	
	for(NSData *testImageData in testDataArray)
	{
		char faviconURL[512];
		snprintf(faviconURL, sizeof(faviconURL), "http://myurl-%d.com/myfavicon.ico", i++);
		result = sqlite3_bind_text(pInsertStatement, 1, faviconURL, -1, SQLITE_STATIC);
		assert(result == SQLITE_OK);
		
		result = sqlite3_bind_blob(pInsertStatement, 2, [testImageData bytes], [testImageData length], SQLITE_STATIC);
		assert(result == SQLITE_OK);
		
		result = sqlite3_step(pInsertStatement);
		assert(result == SQLITE_DONE);
		
		result = sqlite3_reset(pInsertStatement);
		assert(result == SQLITE_OK);
	}
	
	stopTimer(@"Inserted all data");
	
	startTimer();
	
	sqlite3_finalize(pInsertStatement);
	sqlite3_close(pDB);
	
	stopTimer(@"Closed datbase");
	
	[pool drain];
}

static void plistTest()
{
	NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	
	NSMutableDictionary *dictionary = [[[NSMutableDictionary alloc] init] autorelease];
	
	NSString *kPlistOutputFile = @"/tmp/DataStoreComparison-plistTest.plist";
	
	if([[NSFileManager defaultManager] fileExistsAtPath:kPlistOutputFile])
	{
		BOOL result = [[NSFileManager defaultManager] removeItemAtPath:kPlistOutputFile error:nil];
		assert(result);
	}
	
	startTimer();
	
	int i = 0 ;
	
	for(NSData *testImageData in testDataArray)
	{
		NSAutoreleasePool * subpool = [[NSAutoreleasePool alloc] init];
		
		[dictionary setObject:testImageData forKey:[NSString stringWithFormat:@"File%d", i++]];
		//[dictionary writeToFile:@"/tmp/DataStoreComparison-plistTest.plist" atomically:NO];
		
		NSData *plistData = [NSPropertyListSerialization dataFromPropertyList:dictionary
																	   format:NSPropertyListBinaryFormat_v1_0
															 errorDescription:nil];
		
		assert(plistData);
		[plistData writeToFile:kPlistOutputFile atomically:YES];
		
//		if(i % 500 == 0)
//		{
//			stopTimer([NSString stringWithFormat:@"At %d", i]);
//			startTimer();
//		}
		
		[subpool drain];
	}
	
	stopTimer(@"Wrote all data to a property list");
	
	[pool drain];	
}

static NSString* SHA1FilenameFromURLString(NSString *url)
{
	unsigned char md[CC_SHA1_DIGEST_LENGTH];
	CC_SHA1_CTX ctx;
	
	CC_SHA1_Init(&ctx);
	CC_SHA1_Update(&ctx, [url UTF8String], [url lengthOfBytesUsingEncoding:NSUTF8StringEncoding]);
	CC_SHA1_Final(md, &ctx);
	
	return [NSString stringWithFormat:@"%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X",
			md[0], md[1], md[2], md[3], md[4], md[5], md[6], md[7], md[8], md[9],
			md[10], md[11], md[12], md[13], md[14], md[15], md[16], md[17], md[18], md[19]];
}

static void fileSystemTest()
{
	NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	
	NSString *kFileSystemDirectory = @"/tmp/DataStoreComparison-fileSystem";
	
	if([[NSFileManager defaultManager] fileExistsAtPath:kFileSystemDirectory])
	{
		BOOL result = [[NSFileManager defaultManager] removeItemAtPath:kFileSystemDirectory error:nil];
		assert(result);
	}
	
	if(![[NSFileManager defaultManager] createDirectoryAtPath:kFileSystemDirectory withIntermediateDirectories:YES attributes:nil error:nil])
	{
		NSLog(@"Couldn't create file system directory.");
		exit(1);
	}
	
	startTimer();
	
	int i = 0 ;
	
	for(NSData *testImageData in testDataArray)
	{
		NSString *filename = [kFileSystemDirectory stringByAppendingPathComponent:SHA1FilenameFromURLString([NSString stringWithFormat:@"File-%d", i++])];
		
		if(![testImageData writeToFile:filename atomically:YES])
		{
			NSLog(@"Failed to save file in file system test.");
			exit(1);
		}
	}
	
	stopTimer(@"Wrote data to files");
	
	[pool drain];
}

static UnsignedWide startTime;

static void startTimer()
{
	Microseconds(&startTime);
}

static void stopTimer(NSString* message)
{
	UnsignedWide stopTime;
	Microseconds(&stopTime);
	
	SInt64 timeStart = startTime.hi;
	timeStart = (timeStart << 32) | startTime.lo;
	
	SInt64 timeStop = stopTime.hi;
	timeStop = (timeStop << 32) | stopTime.lo;
	
	SInt64 difference = timeStop - timeStart;
	double differenceInSecondes = ((double)difference) / 1000000.0;
	
	NSLog(@"%@: %f seconds.", message, differenceInSecondes);
}
