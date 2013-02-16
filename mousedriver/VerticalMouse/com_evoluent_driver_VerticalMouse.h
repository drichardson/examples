/*
 * Copyright (c) 2008 Douglas Richardson. All rights reserved.
 *
 * Use of this code is subject to a license agreement with Douglas Richardson.
 * dougie.richardson@gmail.com
 */

#ifndef _EVOLUENT_VERTICAL_MOUSE_H_
#define _EVOLUENT_VERTICAL_MOUSE_H_

#include <IOKit/usb/IOUSBHIDDriver.h>

#define kMouseRetryCount	3

class com_evoluent_driver_VerticalMouse : public IOUSBHIDDriver
{
	OSDeclareDefaultStructors(com_evoluent_driver_VerticalMouse)
	
private:
	bool				_switchTo800dpiFlag;
	bool				_switchTo2000fpsFlag;
	bool				_switchBackOnRestart;
	
	// IOService methods - REMOVE THESE
	bool init( OSDictionary * dictionary );
	IOService* probe(IOService * provider, SInt32 * score);
	
	// IOKit methods
	virtual IOReturn	setPowerState ( unsigned long powerStateOrdinal, IOService* whatDevice );
	virtual bool		willTerminate(IOService * provider, IOOptionBits options);
	
	// IOUSBHIDDriver methods
	virtual IOReturn	StartFinalProcessing(void);
	
};

#endif

