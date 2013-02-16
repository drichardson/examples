#import <Foundation/Foundation.h>
#import <Security/Security.h>

static BOOL DXRemoveDeliciousAPICredentials()
{
	SecKeychainItemRef keychainItem = NULL;
	const char* serverName = "api.del.icio.us";
	
	OSStatus err = SecKeychainFindInternetPassword(NULL,
												   strlen(serverName), serverName,
												   0, NULL,
												   0, NULL,
												   0, "",
												   443,
												   kSecProtocolTypeHTTPS,
												   kSecAuthenticationTypeDefault,
												   NULL, NULL, &keychainItem);
	
	if(err == noErr)
	{
		err = SecKeychainItemDelete(keychainItem);
		CFRelease(keychainItem);
		
		if(err != noErr)
			NSLog(@"Error deleting credential (error = %d)", err);
	}
	else
	{
		NSLog(@"Could not find credentials for %s (error = %d)", serverName, err);
	}
	
	return err == noErr ? YES : NO;
}

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	
	DXRemoveDeliciousAPICredentials("mofochickamo");
    
    [pool drain];
    return 0;
}
