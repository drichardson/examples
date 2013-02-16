// Taken from http://developer.apple.com/hardwaredrivers/customusbdrivers.html

#include <stdio.h>
#include <IOKit/IOCFPlugIn.h>
#include <IOKit/hid/IOHIDKeys.h>
#include <CoreFoundation/CoreFoundation.h>

int
main(void) 
{
	IOReturn result = kIOReturnSuccess;
	io_iterator_t hidObjectIterator = 0;
	io_object_t hidDevice = IO_OBJECT_NULL;
	CFMutableDictionaryRef hidMatchDictionary = 0;
	CFMutableDictionaryRef hidProperties = 0;
	
	// Create the IO iterator
	hidMatchDictionary = IOServiceMatching(kIOHIDDeviceKey);
	result = IOServiceGetMatchingServices(kIOMasterPortDefault,
										  hidMatchDictionary,
										  &hidObjectIterator);
	if ((result != kIOReturnSuccess) || (hidObjectIterator == 0)) {
		printf("Can't obtain an IO iterator\n");
		exit(1);
	}
	
	while ((hidDevice = IOIteratorNext(hidObjectIterator)))
	{
		hidProperties = 0;
		int vendor = 0, product = 0;
		result = IORegistryEntryCreateCFProperties(hidDevice, &hidProperties,
												   kCFAllocatorDefault, kNilOptions);
		if ((result == KERN_SUCCESS) && hidProperties)
		{
			CFNumberRef vendorRef, productRef;
			
			vendorRef = CFDictionaryGetValue(hidProperties, CFSTR(kIOHIDVendorIDKey));
			productRef = CFDictionaryGetValue(hidProperties, CFSTR(kIOHIDProductIDKey));
			
			if (vendorRef)
				CFNumberGetValue(vendorRef, kCFNumberIntType, &vendor);	   CFRelease(vendorRef);
			
			if (productRef)
			{
				CFNumberGetValue(productRef, kCFNumberIntType, &product);
				CFRelease(productRef);
			}
		}
		
		printf("Got a device: vendor %04x, product %04x\n", vendor, product);
		IOObjectRelease(hidDevice);
	}
	
	IOObjectRelease(hidObjectIterator);
	return 0;
}