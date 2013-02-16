#include <IOKit/usb/IOUSBHIDDriver.h>

class com_yourcompany_driver_USBHIDDriver1 : public IOUSBHIDDriver
{
	OSDeclareDefaultStructors(com_yourcompany_driver_USBHIDDriver1)
	
public:
	virtual bool init( OSDictionary * dictionary = 0 );
	
	IOService* probe(IOService *provider, SInt32 *score);
	
	// IOUSBHIDDriver methods
    virtual IOReturn StartFinalProcessing(void);
};
