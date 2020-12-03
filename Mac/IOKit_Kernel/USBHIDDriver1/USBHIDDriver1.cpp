#include "USBHIDDriver1.h"

#define super IOUSBHIDDriver

OSDefineMetaClassAndStructors(com_yourcompany_driver_USBHIDDriver1, IOUSBHIDDriver)

bool com_yourcompany_driver_USBHIDDriver1::init(OSDictionary * dictionary)
{
	bool rc = super::init(dictionary);
	IOLog("Yeah, my ::init got called! w00t!\n");
	
	OSCollectionIterator *itr = OSCollectionIterator::withCollection(dictionary);
	if(itr == NULL)
		goto bail;
	
	OSObject *o;
	while((o = itr->getNextObject()) != NULL)
	{
		//IOLog("Got object\n");
		
		//if(OSTypeIDInst(o) == OSTypeID(OSString))
		//{
		//	OSString *str = (OSString*)o;
		//	IOLog("Got string '%s'\n", str->getCStringNoCopy());
		//}
		
		if(OSTypeIDInst(o) == OSTypeID(OSSymbol))
		{
			OSSymbol *sym = (OSSymbol*)o;
			//IOLog("Got symbol '%s'\n", sym->getCStringNoCopy());
			
			o = dictionary->getObject(sym);
			
			if(OSTypeIDInst(o) == OSTypeID(OSString))
			{
				OSString *str = (OSString*)o;
				IOLog("Got symbol '%s'='%s'\n", sym->getCStringNoCopy(), str->getCStringNoCopy());
			}
			else if(OSTypeIDInst(o) == OSTypeID(OSNumber))
			{
				OSNumber *n = (OSNumber*)o;
				IOLog("Got symbol '%s'=%u\n", sym->getCStringNoCopy(), n->unsigned32BitValue());
			}
			else
			{
				IOLog("Got symbol for unknown type: %s", sym->getCStringNoCopy());
			}
		}
	}
	
bail:
	return rc;
}

IOService* 
com_yourcompany_driver_USBHIDDriver1::probe(IOService *provider, SInt32 *score)
{
    IOLog("%s(%p)::probe - in score=%d\n", getName(), this, *score);
    IOService *rc = super::probe(provider, score); // this returns this
	
	// This is cheating. Figure out how to properly get a high score based on matching for USB HID drivers.
	*score = 100000;
	
	return rc;
}

IOReturn com_yourcompany_driver_USBHIDDriver1::StartFinalProcessing(void)
{
	IOLog("Howdy, howdy, howdy! StartFinalProcessing called\n");
	return super::StartFinalProcessing();
}
