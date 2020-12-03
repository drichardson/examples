#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>
#include <CoreFoundation/CoreFoundation.h>

// A test function builds a dictionary request message.
typedef CFDictionaryRef (*TestFunction)(void);
static void testRunner(const char* URL, TestFunction testFunction);

static CFDictionaryRef testFunction_sumValues(void);

int main (int argc, const char * argv[])
{
	if (argc != 2)
	{
		fprintf(stderr, "Usage: http-client <URL>\n");
		exit(1);
	}
	
	const char* url = argv[1];
	
	curl_global_init(CURL_GLOBAL_ALL);
	testRunner(url, testFunction_sumValues);
	curl_global_cleanup();
	
    return 0;
}

static void testRunner(const char* url, TestFunction testFunction)
{
	// Use the same curl handle to perform multiple requests and it may keep the HTTP seession open
	// (the docs say this).
	
	CURL *curlHandle = curl_easy_init();
	if (curlHandle == NULL)
	{
		fprintf(stderr, "curl_easy_init returned NULL. Exiting.");
		exit(1);
	}
	
	curl_easy_setopt(curlHandle, CURLOPT_URL, url);
	
	// pass our list of custom made headers
	struct curl_slist *headers;
	headers = curl_slist_append(NULL, "Content-Type: text/xml");
	curl_easy_setopt(curlHandle, CURLOPT_HTTPHEADER, headers);
	
	int testCount = 0;
	
	
	while (true)
	{		
		CFDictionaryRef request = testFunction();
		
		if (request == NULL || CFGetTypeID(request) != CFDictionaryGetTypeID())
		{
			fprintf(stderr, "Test function returned NULL or isn't a dictionary %p. Exiting.", request);
			exit(1);
		}
		
		CFDataRef xmlData = CFPropertyListCreateXMLData(NULL, request);
		if (xmlData == NULL)
		{
			fprintf(stderr, "Unable to create data out of property list. Exiting.");
			exit(1);
		}
		
		// post binary data
		curl_easy_setopt(curlHandle, CURLOPT_POSTFIELDS, CFDataGetBytePtr(xmlData));
		
		// set the size of the postfields data
		curl_easy_setopt(curlHandle, CURLOPT_POSTFIELDSIZE, CFDataGetLength(xmlData));
		
		CURLcode rc = curl_easy_perform(curlHandle);
		
		if (rc == CURLE_OK)
		{
			printf("Performed test: %d\n", ++testCount);
		}
		else
		{
			fprintf(stderr, "Didn't get CURL OK: %d\n", rc);
		}

		
		CFRelease(xmlData);
		CFRelease(request);
	}
	
	curl_slist_free_all(headers); //free the header list
	curl_easy_cleanup(curlHandle);
}

static CFDictionaryRef testFunction_sumValues(void)
{
	static CFMutableDictionaryRef result = NULL;
	
	if (result == NULL)
	{
		result = CFDictionaryCreateMutable(NULL, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
		
		CFMutableArrayRef valuesToSum = CFArrayCreateMutable(NULL, 0, &kCFTypeArrayCallBacks);
		
		int i;
		for (i = 0; i <= 100; i++)
		{
			CFNumberRef num = CFNumberCreate(NULL, kCFNumberIntType, &i);
			CFArrayAppendValue(valuesToSum, num);
			CFRelease(num);		
		}
		
		CFDictionarySetValue(result, CFSTR("Type"), CFSTR("RetrieveDataInMemory"));
		CFDictionarySetValue(result, CFSTR("ValuesToSum"), valuesToSum);
		CFRelease(valuesToSum);
	}
	
	CFRetain(result);
	
	return result;
}
