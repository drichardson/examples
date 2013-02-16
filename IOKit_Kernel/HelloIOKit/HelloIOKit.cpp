#include <IOKit/IOLib.h>
#include "HelloIOKit.h"

extern "C" {
#include <pexpert/pexpert.h> //This is for debugging purposes ONLY
}

// Define my superclass
#define super IOService

// REQUIRED! This macro defines the class's constructors, destructors,
// and several other methods I/O Kit requires. Do NOT use super as the
// second parameter. You must use the literal name of the superclass.
OSDefineMetaClassAndStructors(com_MyTutorial_driver_HelloIOKit, IOService)

bool com_MyTutorial_driver_HelloIOKit::init(OSDictionary *dict)
{
	bool res = super::init(dict);
	IOLog("Initializing\n");
	return res;
}

void com_MyTutorial_driver_HelloIOKit::free(void)
{
	IOLog("Freeing\n");
	super::free();
}


IOService* com_MyTutorial_driver_HelloIOKit::probe(IOService *provider, SInt32 *score)
{
	IOService *res = super::probe(provider, score);
	IOLog("Probing\n");
	return res;
}

bool com_MyTutorial_driver_HelloIOKit::start(IOService *provider)
{
	bool res = super::start(provider);
	IOLog("Starting\n");
	return res;
}

void com_MyTutorial_driver_HelloIOKit::stop(IOService *provider)
{
	IOLog("Stopping\n");
	super::stop(provider);
}
