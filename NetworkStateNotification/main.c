#include <sys/cdefs.h>
#include <CoreFoundation/CoreFoundation.h>
#include <SystemConfiguration/SystemConfiguration.h>

static void my_SCNetworkReachabilityCallBack(SCNetworkReachabilityRef target,
											 SCNetworkConnectionFlags flags,
											 void *info);

int main (int argc, const char * argv[])
{
	SCNetworkReachabilityRef r = SCNetworkReachabilityCreateWithName(NULL, "api.del.icio.us");
	
	if(r == NULL)
	{
		printf("Error creating reachability reference.\n");
		goto bail;
	}
	
	if(!SCNetworkReachabilitySetCallback(r, my_SCNetworkReachabilityCallBack, NULL))
	{
		printf("Unable to set reachability callback\n");
		goto bail;
	}
	
	if(!SCNetworkReachabilityScheduleWithRunLoop(r, CFRunLoopGetCurrent(), kCFRunLoopCommonModes))
	{
		printf("Unable to schedule run loop monitoring on run loop.\n");
		goto bail;
	}
	
	printf("Starting run loop. Enable and disable network interfaces to fire the callback.\n");
	CFRunLoopRun();
	printf("Run loop stopped\n");
	
	
bail:
	if(r)
		CFRelease(r);
	
    return 0;
}


static void my_SCNetworkReachabilityCallBack(SCNetworkReachabilityRef target,
											 SCNetworkConnectionFlags flags,
											 void *info)
{
	printf("network callback called: 0x%08X\n", flags);
	
	if(flags & kSCNetworkFlagsReachable)
		printf("  network is reachable\n");
	
	if(flags & kSCNetworkFlagsConnectionRequired)
		printf("  can reach but connection is required\n");
	
	// A transient connection is a VPN connection and, perhaps though I have not tested it, a dial-up connection.
	// To test this, enter an address of a machine inside a VPN network you have access to. When you make the VPN connection
	// this flag should be set as well as the kSCNetworkFlagsReachable flag. When the connection is
	// terminated, flags will be 0.
	if(flags & kSCNetworkFlagsTransientConnection)
		printf("  can reach network through transient connection\n");
	
	if(flags & kSCNetworkFlagsConnectionAutomatic)
		printf("  connect needs to be established, but a communication attempt will try to establish the required connection\n");
	
	if(flags & kSCNetworkFlagsInterventionRequired)
		printf("  could connect, but user intervention required.\n");
	
	if(flags & kSCNetworkFlagsIsLocalAddress)
		printf("  is local address\n");
	
	if(flags & kSCNetworkFlagsIsDirect)
		printf("  is direct\n");
	
	
	
	// Higher level translation
	if((flags & kSCNetworkFlagsReachable) && !(flags & kSCNetworkFlagsConnectionRequired) && !(flags & kSCNetworkFlagsIsDirect))
		printf("  STATUS: GOOD TO GO\n");
	else
		printf("  STATUS: NOT CONNECTED\n");
}
