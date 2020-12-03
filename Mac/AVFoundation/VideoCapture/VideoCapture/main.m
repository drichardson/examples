//
//  main.m
//  VideoCapture
//
//  Created by Doug Richardson on 8/23/11.
//  Copyright (c) 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <AVFoundation/AVFoundation.h>
#import <stdio.h>

@interface MyRecordingDelegate : NSObject <AVCaptureFileOutputRecordingDelegate, AVCaptureFileOutputDelegate>
@property (strong) AVCaptureMovieFileOutput* output;
@property Float64 maxSegmentDuration;
@end

@implementation MyRecordingDelegate
{
    NSUInteger _sequence;
    Float64 _lastDuration;
}

@synthesize output, maxSegmentDuration;

- (void)next
{
    NSString* path = [NSString stringWithFormat:@"/Users/doug/Desktop/VIDEO_OUTPUT/file_%d.m4v", _sequence];
    _sequence++;
    
    NSURL* url = [NSURL fileURLWithPath:path];
    NSLog(@"Start recording to %@", url);
    
    [self.output startRecordingToOutputFileURL:url recordingDelegate:self];
}

- (void)start
{
    _sequence = 0;
    _lastDuration = 0;
    [self next];
}

- (void)captureOutput:(AVCaptureFileOutput *)captureOutput didOutputSampleBuffer:(CMSampleBufferRef)sampleBuffer fromConnection:(AVCaptureConnection *)connection NS_AVAILABLE(10_7, NA)
{
    assert(captureOutput == self.output);
    
    Float64 duration = CMTimeGetSeconds(captureOutput.recordedDuration);
    NSLog(@"Did output sample buffer. Duration is %f", duration);
    
    Float64 maxSegmentDurationWithFudge = self.maxSegmentDuration - 0.25;
   
    if ( _lastDuration <= maxSegmentDurationWithFudge && duration > maxSegmentDurationWithFudge )
    {
        [self next];
    }
    
    _lastDuration = duration;
}

- (void)captureOutput:(AVCaptureFileOutput *)captureOutput willFinishRecordingToOutputFileAtURL:(NSURL *)fileURL fromConnections:(NSArray *)connections error:(NSError *)error
{
    NSLog(@"Capture output will finish. Error: %@", error);
}

- (void)captureOutput:(AVCaptureFileOutput *)captureOutput didFinishRecordingToOutputFileAtURL:(NSURL *)outputFileURL fromConnections:(NSArray *)connections error:(NSError *)error
{
    NSLog(@"Capture output called. Error: %@", error);
    // TODO: Run the TS wrapper code
}

@end

int main (int argc, const char * argv[])
{

    @autoreleasepool {
        
        AVCaptureDevice* device = [AVCaptureDevice defaultDeviceWithMediaType:AVMediaTypeVideo];
        if ( device == nil )
        {
            NSLog(@"Couldn't find video capture device.");
            exit(1);
        }
        
        NSError* error = nil;
        AVCaptureDeviceInput* input = [AVCaptureDeviceInput deviceInputWithDevice:device error:&error];
        if ( input == nil )
        {
            NSLog(@"Couldn't get input source for device. %@", error);
            exit(1);
        }
        
        AVCaptureSession* session = [AVCaptureSession new];
        
        if ( ![session canSetSessionPreset:AVCaptureSessionPresetHigh] )
        {
            NSLog(@"Couldn't set session preset to high");
            exit(1);
        }
        
        session.sessionPreset = AVCaptureSessionPresetHigh;
        
        if ( ![session canAddInput:input] )
        {
            NSLog(@"Couldn't add input to session");
            exit(1);
        }
            
        [session addInput:input];
        
        AVCaptureMovieFileOutput* output = [AVCaptureMovieFileOutput new];
        
        if ( ![session canAddOutput:output] )
        {
            NSLog(@"Couldn't add output to session");
            exit(1);
        }
        
        [session addOutput:output];
        
        [session startRunning];
        
        NSLog(@"Do I have a delegate: %@", output.delegate);
        MyRecordingDelegate* myDelegate = [MyRecordingDelegate new];
        myDelegate.output = output;
        myDelegate.maxSegmentDuration = 10;
        output.delegate = myDelegate;
        [myDelegate start];
        
        dispatch_main();
        
        puts("ok");
        
    }
    return 0;
}

