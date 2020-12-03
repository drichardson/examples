#import <Cocoa/Cocoa.h>
#import <QTKit/QTKit.h>

@interface AppController : NSObject {
	IBOutlet NSButton *videoButton;
	IBOutlet NSButton *voiceButton;
	IBOutlet QTCaptureView *captureView;
	QTCaptureSession *mCaptureSession;
	QTCaptureMovieFileOutput *mCaptureMovieFileOutput;
	QTCaptureDeviceInput *mCaptureVideoDeviceInput;
	QTCaptureDeviceInput *mCaptureAudioDeviceInput;
}

-(IBAction)captureVideo:(id)sender;
-(IBAction)captureVoice:(id)sender;
-(IBAction)capturePhoto:(id)sender;

@end
