#include <fcgi_stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <CoreFoundation/CoreFoundation.h>

extern char **environ;

static void handleRequest(void);
static CFDictionaryRef parseRequest(void);
static CFDictionaryRef processRequest(CFDictionaryRef request);
static void sendResponse(CFDictionaryRef response);
static void sendErrorResponse(void);

static CFDictionaryRef handleRetrieveDataInMemoryRequest(CFDictionaryRef request);

int main (int argc, const char * argv[])
{	
	while (FCGI_Accept() >= 0)
	{
		handleRequest();
	}
	
    return 0;
}

static void handleRequest(void)
{
	CFDictionaryRef request = parseRequest();
	bool error = true;
	
	if (request)
	{
		CFDictionaryRef response = processRequest(request);
		
		if (response)
		{
			sendResponse(response);
			
			error = false;
			CFRelease(response);
		}
		
		CFRelease(request);
	}
	
	if (error)
	{
		sendErrorResponse();
	}
}

static CFDictionaryRef parseRequest(void)
{	
	CFDictionaryRef result = NULL;
	const char* requestMethod = getenv("REQUEST_METHOD");
	
	if(requestMethod && strcmp(requestMethod, "POST") == 0)
	{
		CFMutableDataRef data = CFDataCreateMutable(NULL, 0);
		
		char buf[10000];
		while (!feof(stdin))
		{
			int count = fread(buf, 1, sizeof(buf), stdin);
			
			if (count > 0)
			{
				CFDataAppendBytes(data, (UInt8*)buf, count);
			}
		}
		
		CFDictionaryRef dict = CFPropertyListCreateFromXMLData(NULL, data, kCFPropertyListImmutable, NULL);
		
		CFRelease(data);
		
		// Try to parse as a dictionary.
		
		if (dict && CFGetTypeID(dict) == CFDictionaryGetTypeID())
		{
			result = dict; // Result now owns dict.
		}
		else
		{
			if (dict)
			{
				CFRelease(dict);
			}
		}
	}
	
	return result;
}

static CFDictionaryRef processRequest(CFDictionaryRef request)
{
	CFDictionaryRef result = NULL;
	CFStringRef requestType = CFDictionaryGetValue(request, CFSTR("Type"));
	
	if (requestType == NULL || CFGetTypeID(requestType) != CFStringGetTypeID())
	{
		goto error;
	}
	
	if (CFEqual(requestType, CFSTR("SumValues")))
	{
		result = handleRetrieveDataInMemoryRequest(request);
	}
	else
	{
		goto error;
	}

	
	return result;
	
error:
	return NULL;
}

static void sendResponse(CFDictionaryRef response)
{
	printf("Content-type: text/xml\r\n\r\n");
	
	CFDataRef data = CFPropertyListCreateXMLData(NULL, response);
	
	fwrite((void*)CFDataGetBytePtr(data), 1, CFDataGetLength(data), stdout);
	
	if (data)
		CFRelease(data);
}

static void sendErrorResponse(void)
{
	CFMutableDictionaryRef dict = CFDictionaryCreateMutable(NULL, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
	CFDictionarySetValue(dict, CFSTR("code"), CFSTR("error"));
	sendResponse(dict);
	CFRelease(dict);
}

static CFDictionaryRef handleRetrieveDataInMemoryRequest(CFDictionaryRef request)
{
	CFMutableDictionaryRef result = CFDictionaryCreateMutable(NULL, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
	
	double sum = 0;
	CFArrayRef valuesToSum = CFDictionaryGetValue(request, CFSTR("ValuesToSum"));
	if (valuesToSum && CFGetTypeID(valuesToSum) == CFArrayGetTypeID())
	{
		CFIndex count = CFArrayGetCount(valuesToSum);
		CFIndex i;
		for (i = 0; i < count; ++i)
		{
			CFNumberRef valueRef = CFArrayGetValueAtIndex(valuesToSum, i);
			if (valueRef && CFGetTypeID(valueRef) == CFNumberGetTypeID())
			{
				double value;
				CFNumberGetValue(valueRef, kCFNumberDoubleType, &value);
				sum += value;
			}
		}
	}
	
	
	CFDictionarySetValue(result, CFSTR("code"), CFSTR("ok"));
	
	CFNumberRef sumRef = CFNumberCreate(NULL, kCFNumberDoubleType, &sum);
	CFDictionarySetValue(result, CFSTR("result"), sumRef);
	CFRelease(sumRef);
	
	return result;
}
