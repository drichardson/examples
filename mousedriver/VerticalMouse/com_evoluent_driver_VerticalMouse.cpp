/*
 * Copyright (c) 2008 Douglas Richardson. All rights reserved.
 *
 * Use of this code is subject to a license agreement with Douglas Richardson.
 * dougie.richardson@gmail.com
 */

//#include <IOKit/IOLib.h>
#include <IOKit/usb/IOUSBLog.h>
#include <IOKit/IOMessage.h>
#include <IOKit/pwr_mgt/RootDomain.h>

#define FORMOUSETESTING 0

#include "com_evoluent_driver_VerticalMouse.h"

//extern "C" {
//#include <pexpert/pexpert.h> //This is for debugging purposes ONLY
//}


#define super IOUSBHIDDriver

#define kDefaultFixedResolution (400 << 16)

/* Convert USBLog to use kprintf debugging */
#define VerticalMouse_USE_KPRINTF 0

#if VerticalMouse_USE_KPRINTF
#undef USBLog
#undef USBError
void kprintf(const char *format, ...)
__attribute__((format(printf, 1, 2)));
#define USBLog( LEVEL, FORMAT, ARGS... )  if ((LEVEL) <= VerticalMouse_USE_KPRINTF) { kprintf( FORMAT "\n", ## ARGS ) ; }
#define USBError( LEVEL, FORMAT, ARGS... )  { kprintf( FORMAT "\n", ## ARGS ) ; }
#endif
OSDefineMetaClassAndStructors(com_evoluent_driver_VerticalMouse, super)

static	bool switchTo800dpi = true;

bool
com_evoluent_driver_VerticalMouse::init(OSDictionary * dictionary)
{
	printf("com_evoluent_driver_VerticalMouse::init YEAH!\n");
	IOLog("1: com_evoluent_driver_VerticalMouse::init YEAH!\n");
	return super::init(dictionary);
}

IOService*
com_evoluent_driver_VerticalMouse::probe(IOService * provider, SInt32 * score)
{
	printf("com_evoluent_driver_VerticalMouse::probe YEAH!\n");
	IOLog("2: com_evoluent_driver_VerticalMouse::probe YEAH!\n");
	return super::probe(provider, score);
}

IOReturn
com_evoluent_driver_VerticalMouse::StartFinalProcessing()
{
    OSNumber 		*curResPtr, *resPrefPtr;
    UInt32			curResInt, resPrefInt;
    IOFixed			curRes, resPref;
    IOReturn		err = kIOReturnSuccess;
    OSBoolean * 	boolObj;
    OSObject *		propertyObj = NULL;
	
    USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing", this);
	_switchBackOnRestart = FALSE;
    propertyObj = copyProperty("SwitchTo800DPI");
    boolObj = OSDynamicCast( OSBoolean, propertyObj );
    if ( boolObj && boolObj->isTrue() )
    {
		// USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - found switchTo800DPI resolution property", this);
        _switchTo800dpiFlag = true;
    }
	if (propertyObj)
		propertyObj->release();
	
    propertyObj = copyProperty("SwitchTo2000FPS");
    boolObj = OSDynamicCast( OSBoolean, propertyObj );
    if ( boolObj && boolObj->isTrue() )
    {
		// USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - found switchTo2000fps resolution property", this);
        _switchTo2000fpsFlag = true;
    }
	if (propertyObj)
		propertyObj->release();
	
    if ( _switchTo2000fpsFlag )
    {
        IOUSBDevRequest		devReq;
        
        // Write the 2000 FPS value to the mouse
        //
        devReq.bmRequestType = 0x40;
        devReq.bRequest = 0x01;
        devReq.wValue = 0x05AC;
        devReq.wIndex = 0xd810;
        devReq.wLength = 0x0000;
        devReq.pData = NULL;
		
        err = _device->DeviceRequest(&devReq, 5000, 0);
		
        if (err)
		{
            USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - sending 1st part of FPS change received error 0x%x", this, err);
		}
        else
        {
            devReq.bmRequestType = 0x40;
            devReq.bRequest = 0x01;
            devReq.wValue = 0x05AC;
            devReq.wIndex = 0xdc11;
            devReq.wLength = 0x0000;
            devReq.pData = NULL;
			
            err = _device->DeviceRequest(&devReq, 5000, 0);
			
            if (err)
			{
                USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - sending 2nd part of FPS change received error 0x%x", this, err);
			}
        }
		
#if FORMOUSETESTING
        UInt8			hi,lo;
        UInt16			fps;
		
        // Read back the value:
        //
        devReq.bmRequestType = 0xc0;
        devReq.bRequest = 0x01;
        devReq.wValue = 0x05AC;
        devReq.wIndex = 0x0011;
        devReq.wLength = 1;
        devReq.pData = &hi;
		
        err = _device->DeviceRequest(&devReq, 5000, 0);
        if (err)
            USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - error reading hi byte: 0x%x", this, err);
		
        devReq.bmRequestType = 0xc0;
        devReq.bRequest = 0x01;
        devReq.wValue = 0x05AC;
        devReq.wIndex = 0x0010;
        devReq.wLength = 1;
        devReq.pData = &lo;
		
        err = _device->DeviceRequest(&devReq, 5000, 0);
        if (err)
            USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - read reading lo byte: 0x%x", this, err);
		
        fps = hi;
        fps = (fps << 8) | lo;
		
        USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - read : 0x%x", this, fps );
		
#endif
        
        err = super::StartFinalProcessing();
        if (err)
		{
            USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - super returned error 0x%x", this, err);
		}
		
    }
	
    if ( _switchTo800dpiFlag )
    {
		propertyObj = copyProperty(kIOHIDPointerResolutionKey);
        curResPtr = OSDynamicCast( OSNumber, propertyObj );
        if (curResPtr)
        {
            curResInt = curResPtr->unsigned32BitValue();
            USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - found current resolution property - value 0x%lx", this, curResInt);
        }
        else
        {
            curResInt = kDefaultFixedResolution;
            USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - no current property found - using default 0x%lx", this, curResInt);
        }
		if (propertyObj)
			propertyObj->release();
		
		propertyObj = copyProperty(("xResolutionPref"));
        resPrefPtr = OSDynamicCast( OSNumber, propertyObj );
        if (resPrefPtr)
            resPrefInt = resPrefPtr->unsigned32BitValue();
        else
        {
            resPrefInt = kDefaultFixedResolution * 2;
            USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - no preference property found - using default 0x%lx", this, resPrefInt);
        }
		if (propertyObj)
			propertyObj->release();
		
        resPref = (IOFixed) resPrefInt;
        curRes = (IOFixed) curResInt;
		
        if (resPref != curRes)
        {
            if (switchTo800dpi)
            {
                IOUSBDevRequest		devReq;
				
                devReq.bmRequestType = 0x40;
                devReq.bRequest = 0x01;
                devReq.wValue = 0x05AC;
                devReq.wIndex = 0x0452;
                devReq.wLength = 0x0000;
                devReq.pData = NULL;
				
                err = _device->DeviceRequest(&devReq, 5000, 0);
				
                if (err)
				{
                    USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - error (%x) setting resolution", this, err);
				}
                else
				{
					// with this mouse, we do NOT want to start reading on the interrupt pipe, nor do
					// we want to call super::start. We just want to wait for the device to get terminated
                    USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - waiting for click mouse termination", this);
				}
            }
        }
        else
        {
            // If we are already at the correct resolution for OSX, OK. But what if we are going
            // back to OS 9? On restart, switch back to boot setup. Power Manager will tell us
            // when we are going to restart.
            //
			_switchBackOnRestart = TRUE;
            err = super::StartFinalProcessing();
            if (err)
            {
				USBLog(1, "com_evoluent_driver_VerticalMouse[%p]::StartFinalProcessing - error (%p) from super::StartFinalProcessing", this, (void*)err);
            }
        }
    }
    
    return err;
}




bool
com_evoluent_driver_VerticalMouse::willTerminate( IOService * provider, IOOptionBits options )
{
    // this method is intended to be used to stop any pending I/O and to make sure that 
    // we have begun getting our callbacks in order. by the time we get here, the 
    // isInactive flag is set, so we really are marked as being done. we will do in here
    // what we used to do in the message method (this happens first)
    USBLog(3, "com_evoluent_driver_VerticalMouse[%p]::willTerminate isInactive = %d", this, isInactive());
    
    return super::willTerminate(provider, options);
}


IOReturn
com_evoluent_driver_VerticalMouse::setPowerState ( unsigned long powerStateOrdinal, IOService* whatDevice )
{
	USBLog(5, "com_evoluent_driver_VerticalMouse[%p]::setPowerState- powerStateOrdinal[%d] _switchBackOnRestart[%s]", this, (int)powerStateOrdinal, _switchBackOnRestart ? "true" : "false");
	if ((powerStateOrdinal == kUSBHIDPowerStateRestart) && _switchBackOnRestart)
	{
		IOUSBDevRequest		devReq;
		IOReturn			err;
		
		// Tell the driver (using a static variable that will survive across termination)
		// that we don't want to switch to 800 dpi on the next driver start
		//
		switchTo800dpi = false;
		
		// Send switch back command.
		devReq.bmRequestType = 0x40;
		devReq.bRequest = 0x01;
		devReq.wValue = 0x05AC;
		devReq.wIndex = 0x0052;		// switch = 0452; switchback = 0052
		devReq.wLength = 0x0000;
		devReq.pData = NULL;
        
		USBLog(5, "com_evoluent_driver_VerticalMouse[%p]::setPowerState - issuing command to switch back", this);
		err = _device->DeviceRequest(&devReq, 5000, 0);
		if (err)
		{
			USBLog(1, "com_evoluent_driver_VerticalMouse[%p]::setPowerState - err (%p) on DeviceRequest", this, (void*)err);
		}
		else
		{
			USBLog(7, "com_evoluent_driver_VerticalMouse[%p]::setPowerState - command done with no err", this);
		}
	}
	return super::setPowerState(powerStateOrdinal, whatDevice);
}

