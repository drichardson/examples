// Taken from http://developer.apple.com/hardwaredrivers/customusbdrivers.html

#include <stdio.h>
#include <IOKit/IOCFPlugIn.h>
#include <IOKit/hid/IOHIDKeys.h>
#include <IOKit/hid/IOHIDLib.h>
#include <CoreFoundation/CoreFoundation.h>

static io_object_t FindUSBDeviceByVendorIDAndProductID(int vendorID, int productID);
static void ReaderReportCallback(void *target, IOReturn result, void *refcon, void *sender, uint32_t size);

struct reader {
	io_object_t ioObject;
	IOHIDDeviceInterface122 **interface;
	unsigned char buffer[338];
};

int
main(void) 
{
	const int kEvoluentVerticalMouse3Rev2_VendorID = 0x1a7c;
	const int kEvoluentVerticalMouse3Rev2_ProductID = 0x68;
	io_object_t hidDevice = FindUSBDeviceByVendorIDAndProductID(kEvoluentVerticalMouse3Rev2_VendorID,
																kEvoluentVerticalMouse3Rev2_ProductID);
	
	if(!hidDevice)
	{
		printf("Did not find Evoluent VerticalMouse3 Rev 2\n");
		exit(1);
	}
	
	SInt32 score;
	IOCFPlugInInterface **plugInInterface;
	CFRunLoopSourceRef eventSource;
	mach_port_t port;
	struct reader *r;
	
	r = malloc(sizeof(*r));
	r->ioObject = hidDevice;
	IOCreatePlugInInterfaceForService(hidDevice, kIOHIDDeviceUserClientTypeID,
									  kIOCFPlugInInterfaceID, &plugInInterface, &score);
	(*plugInInterface)->QueryInterface(plugInInterface,
									   CFUUIDGetUUIDBytes(kIOHIDDeviceInterfaceID), (LPVOID) &(r->interface));
	(*plugInInterface)->Release(plugInInterface);
	(*(r->interface))->open(r->interface, 0);
	(*(r->interface))->createAsyncPort(r->interface, &port);
	(*(r->interface))->createAsyncEventSource(r->interface, &eventSource);	
	(*(r->interface))->setInterruptReportHandlerCallback(r->interface, r->buffer, 338, ReaderReportCallback, r, NULL);
	(*(r->interface))->startAllQueues(r->interface);
	
	CFRunLoopAddSource(CFRunLoopGetCurrent(), eventSource, kCFRunLoopDefaultMode);
	
	SInt32 reason;
	do {
		reason = CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.1, false);
	} while(reason == kCFRunLoopRunTimedOut);
	
	if(hidDevice)
		IOObjectRelease(hidDevice);
	
	return 0;
}

static io_object_t
FindUSBDeviceByVendorIDAndProductID(int vendorID, int productID)
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
		
		if(vendor == vendorID && product == productID)
		{
			printf("Found matching device\n");
			break;
		}
		
		IOObjectRelease(hidDevice);
	}
	
	IOObjectRelease(hidObjectIterator);
	
	return hidDevice;
}

static void
ReaderReportCallback(void *target, IOReturn result,
					 void *refcon, void *sender, uint32_t size)
{	
	struct reader *r = target;
	printf("ReaderReportCallback: size: %d:", size);
	int i;
	for(i = 0; i < size; ++i)
		printf("%02X", r->buffer[i]);
	
	printf(":");
	
	// Looks like the format of the report is:
	// | Button | X Move Low | X Move High | Y Move Low | Y Move High | Unknown |
	int16_t x,  y;
	uint8_t button;
	button = r->buffer[0];
	x = r->buffer[1] | (r->buffer[2] << 8);
	y = r->buffer[3] | (r->buffer[4] << 8);
	
	printf("button=%u, x=%d, y=%d\n", button, x, y);
}
