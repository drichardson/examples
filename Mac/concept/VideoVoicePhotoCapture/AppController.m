#import "AppController.h"

@implementation AppController

- (void)awakeFromNib 
{ 
	//Create the capture session 
	mCaptureSession = [[QTCaptureSession alloc] init]; 
	//Connect inputs and outputs to the session 
	BOOL success = NO; 
	NSError *error; 
	// Find a video device 
	QTCaptureDevice *videoDevice = [QTCaptureDevice defaultInputDeviceWithMediaType:QTMediaTypeVideo]; 
	success = [videoDevice open:&error];
	
	if (!success)
	{
		// Couldn't find a video device, look for a muxed device.
		videoDevice = [QTCaptureDevice defaultInputDeviceWithMediaType:QTMediaTypeMuxed]; 
		success = [videoDevice open:&error]; 
	}
	
	if (!success)
	{ 
		videoDevice = nil;
		// Handle error
		NSLog(@"NO VIDEO DEVICES!");
	} 
		
	if (videoDevice) { 
		
		if (!success) { 
			// Handle error 
		} 
		
		// Add the video device to the session as device input 
		mCaptureVideoDeviceInput = [[QTCaptureDeviceInput alloc] initWithDevice:videoDevice]; 
		success = [mCaptureSession addInput:mCaptureVideoDeviceInput error:&error]; 
		if (!success) { 
			// Handle error 
		} 
		
		// If the video device doesn't also supply audio, add an audio device input to the session 
		if (![videoDevice hasMediaType:QTMediaTypeSound] && ![videoDevice hasMediaType:QTMediaTypeMuxed])
		{
			QTCaptureDevice *audioDevice = [QTCaptureDevice defaultInputDeviceWithMediaType:QTMediaTypeSound]; 
			success = [audioDevice open:&error];
			if (!success)
			{
				NSLog(@"Couldn't open audio device.");
				audioDevice = nil;
				// Handle error
			}
			
			if (audioDevice)
			{ 
				mCaptureAudioDeviceInput = [[QTCaptureDeviceInput alloc] 
											initWithDevice:audioDevice]; 
				success = [mCaptureSession addInput:mCaptureAudioDeviceInput 
											  error:&error]; 
				if (!success)
				{ 
					// Handle error 
					NSLog(@"Couldn't add audio device to session input. %@", [error localizedDescription]);
				}
			}
		}		
		
		// Create the movie file output and add it to the session 
		mCaptureMovieFileOutput = [[QTCaptureMovieFileOutput alloc] init]; 
		success = [mCaptureSession addOutput:mCaptureMovieFileOutput error:&error]; 
		if (!success) { 
			// Handle error 
		} 
		// Set the controller be the movie file output delegate. 
		[mCaptureMovieFileOutput setDelegate:self]; 
		// Associate the capture view in the UI with the session 
		[captureView setCaptureSession:mCaptureSession]; 
	} 
#if 0
	// Start the capture session running 
	[mCaptureSession startRunning]; 
#endif
} 

-(void)dealloc
{
	[mCaptureSession release];
	[mCaptureVideoDeviceInput release];
	[mCaptureAudioDeviceInput release];
	[mCaptureMovieFileOutput release];
	[super dealloc];
}

-(IBAction)captureVideo:(id)sender
{
	NSLog(@"Capture Video");
	if([[videoButton title] isEqual:@"Stop Video"])
	{
		[mCaptureMovieFileOutput recordToOutputFileURL:nil];
		[mCaptureSession stopRunning];
		[videoButton setTitle:@"Capture Video"];
	}
	else
	{
		[mCaptureSession startRunning];
		[mCaptureMovieFileOutput recordToOutputFileURL:[NSURL fileURLWithPath:[@"~/Desktop/My Movie Capture.mov" stringByExpandingTildeInPath]]];
		[videoButton setTitle:@"Stop Video"];
	}
}

-(void)windowWillClose:(NSNotification*)notification
{
	[mCaptureSession stopRunning];
	[[mCaptureVideoDeviceInput device] close];
	[[mCaptureAudioDeviceInput device] close];
}

-(IBAction)captureVoice:(id)sender;
{
	NSLog(@"Capture Voice");
}


-(IBAction)capturePhoto:(id)sender;
{
	NSLog(@"Capture Photo");
}

- (void)captureOutput:(QTCaptureFileOutput *)captureOutput 
didFinishRecordingToOutputFileAtURL:(NSURL *)outputFileURL forConnections:(NSArray*)connections
		   dueToError:(NSError *)error 
{ 
	[[NSWorkspace sharedWorkspace] openURL:outputFileURL]; 
	// Do something with the movie at /Users/Shared/My Recorded Movie.mov 
} 


@end
