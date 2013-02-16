#import <Foundation/Foundation.h>
#import <SecurityFoundation/SFAuthorization.h>

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	AuthorizationItem items[1];
	
	const char* tool = "/bin/ls";
	
	items[0].name = kAuthorizationRightExecute;
	items[0].value = (void*)tool;
	items[0].valueLength = strlen(items[0].value);
	items[0].flags = 0;
	
	AuthorizationRights rights;
	rights.count = 1;
	rights.items = items;
	
	SFAuthorization *sfAuth = [SFAuthorization authorizationWithFlags:kAuthorizationFlagInteractionAllowed | kAuthorizationFlagExtendRights
															   rights:&rights
														  environment:kAuthorizationEmptyEnvironment];
	AuthorizationRef authRef = [sfAuth authorizationRef];
	
	NSLog(@"Got back an sfAuth = %@", sfAuth);
	NSLog(@"Authorization reference: %p", [sfAuth authorizationRef]);
	
	if(authRef)
	{
		NSLog(@"Executing tool as root");
		
		const char* argv[] = {
			"-l",
			"/var/db/RemoteManagement",
			NULL
		};
		
		FILE *fpIO = NULL;
		
		OSStatus err = AuthorizationExecuteWithPrivileges([sfAuth authorizationRef], tool, kAuthorizationFlagDefaults, (char* const*)argv, &fpIO);
		
		if(err != errAuthorizationSuccess)
		{
			NSLog(@"Error executing tool with root privileges.");
			goto bail;
		}
		
		char buf[512];
		while(!feof(fpIO))
		{
			size_t bytesRead = fread(buf, 1, sizeof(buf) - 1, fpIO);
			buf[bytesRead] = 0;
			
			if(bytesRead > 0)
				printf(buf);
		}
		
	bail:
		if(fpIO)
			fclose(fpIO);
		;
	}
	
    [pool drain];
    return 0;
}
