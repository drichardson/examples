//
//  DominantFrequencyDetector.m
//  DominantFrequencyDetector
//
//  Created by Doug on 3/23/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "DominantFrequencyDetector.h"
#include <AudioToolbox/AudioToolbox.h>

// custom data structure "MyRecorder"
// data we need during callback functions.

#define kNumberRecordBuffers	3

typedef struct MyRecorder {
	AudioQueueRef				queue;
	
	CFAbsoluteTime				queueStartStopTime;
	AudioFileID					recordFile;
	SInt64						recordPacket; // current packet number in record file
	BOOL						running;
} MyRecorder;

static void MyInputBufferHandler(void *                          inUserData,
								 AudioQueueRef                   inAQ,
								 AudioQueueBufferRef             inBuffer,
								 const AudioTimeStamp *          inStartTime,
								 UInt32							inNumPackets,
								 const AudioStreamPacketDescription *inPacketDesc);

static int MyComputeRecordBufferSize(const AudioStreamBasicDescription *format, AudioQueueRef queue, float seconds);
static OSStatus MyGetDefaultInputDeviceSampleRate(Float64 *outSampleRate);


@implementation DominantFrequencyDetector

@synthesize dominantFrequencyInHz;

-(id)init
{
	self = [super init];
	if(self)
	{
		mDetectorThreadIsFinished = YES;
		dominantFrequencyInHz = [NSNumber numberWithInt:0];
		
		OSStatus rc = MPCreateSemaphore(1, 0, &detectorThreadRunningSempahore);
		if(rc)
		{
			NSLog(@"Error creating semaphore for Dominant Frequency Detector. Error code: %d", rc);
			[self release];
			self = nil;
		}
	}
	
	return self;
}

-(void)dealloc
{	
	[dominantFrequencyInHz release];
	dominantFrequencyInHz = nil;
	
	OSStatus rc = MPDeleteSemaphore(detectorThreadRunningSempahore);
	if(rc)
		NSLog(@"Error deleting semaphore for Dominant Frequency Detector. Error code: %d", rc);
	
	[super dealloc];
}

-(void)startDetecting
{
	if(!mDetectorThreadIsFinished)
		return;
	
	[[NSNotificationCenter defaultCenter] postNotificationName:kDominantFrequencyDetector_FrequencyChangedNotification object:self];
	
	mRunDectectorThread = YES;
	mDetectorThreadIsFinished = NO;
	[NSThread detachNewThreadSelector:@selector(detectorThread:) toTarget:self withObject:nil];
}

-(void)stopDetecting
{
	if(!mRunDectectorThread)
		return;
	
	mRunDectectorThread = NO;
	MPSignalSemaphore(detectorThreadRunningSempahore);
}

-(void)detectorThread:(id)unused
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	
	// Setup the read queue.
	
	
	// Run the run loop while we should still be running.
	int i;
	UInt32 bufferByteSize;
	AudioStreamBasicDescription recordFormat;
	MyRecorder aqr;
	
	// fill structures with 0/NULL
	memset(&recordFormat, 0, sizeof(recordFormat));
	memset(&aqr, 0, sizeof(aqr));
	
	// adapt record format to hardware and apply defaults
	if (recordFormat.mSampleRate == 0.)
		MyGetDefaultInputDeviceSampleRate(&recordFormat.mSampleRate);
	
	NSLog(@"Defaults: Sample Rate: %f Hz, bpc: %d, cpf: %d", recordFormat.mSampleRate, recordFormat.mBitsPerChannel, recordFormat.mChannelsPerFrame);
	
	if (recordFormat.mChannelsPerFrame == 0)
		recordFormat.mChannelsPerFrame = 2;
	
	if (recordFormat.mFormatID == 0 || recordFormat.mFormatID == kAudioFormatLinearPCM) {
		// default to PCM, 16 bit int
		recordFormat.mFormatID = kAudioFormatLinearPCM;
		recordFormat.mFormatFlags = kLinearPCMFormatFlagIsSignedInteger | kLinearPCMFormatFlagIsPacked;
		recordFormat.mBitsPerChannel = 16;
#ifdef BIG_ENDIAN
		recordFormat.mFormatFlags |= kLinearPCMFormatFlagIsBigEndian;
#endif
		recordFormat.mBytesPerPacket = recordFormat.mBytesPerFrame = (recordFormat.mBitsPerChannel / 8) * recordFormat.mChannelsPerFrame;
		recordFormat.mFramesPerPacket = 1;
		recordFormat.mReserved = 0;
	}
	
	// create the queue
	OSStatus rc;
	rc = AudioQueueNewInput(&recordFormat,
							MyInputBufferHandler,
							&aqr /* userData */,
							NULL /* run loop */, NULL /* run loop mode */,
							0 /* flags */, &aqr.queue);
	
	if(rc)
	{
		NSLog(@"Failed to create audio queue");
		goto cleanup;
	}
	
#if 0
	// get the record format back from the queue's audio converter --
	// the file may require a more specific stream description than was necessary to create the encoder.
	size = sizeof(recordFormat);
	rc = AudioQueueGetProperty(aqr.queue, kAudioConverterCurrentOutputStreamDescription, &recordFormat, &size);
	if(rc)
	{
		NSLog(@"AudioQueueGetProperty failed");
		goto cleanup;
	}
#endif
	
	// allocate and enqueue buffers
	bufferByteSize = MyComputeRecordBufferSize(&recordFormat, aqr.queue, 0.5);	// enough bytes for half a second
	for (i = 0; i < kNumberRecordBuffers; ++i) {
		AudioQueueBufferRef buffer;
		rc = AudioQueueAllocateBuffer(aqr.queue, bufferByteSize, &buffer);
		if(rc)
		{
			NSLog(@"AudioQueueAllocateBuffer failed");
			goto cleanup;
		}
		
		rc = AudioQueueEnqueueBuffer(aqr.queue, buffer, 0, NULL);
		if(rc)
		{
			NSLog(@"AudioQueueEnqueueBuffer failed");
			goto cleanup;
		}
	}
	
#if 0
	// record
	if (seconds > 0) {
		// user requested a fixed-length recording (specified a duration with -s)
		// to time the recording more accurately, watch the queue's IsRunning property
		CheckError(AudioQueueAddPropertyListener(aqr.queue, kAudioQueueProperty_IsRunning,
												 MyPropertyListener, &aqr), "AudioQueueAddPropertyListener failed");
		
		// start the queeue
		aqr.running = TRUE;
		CheckError(AudioQueueStart(aqr.queue, NULL), "AudioQueueStart failed");
		CFAbsoluteTime waitUntil = CFAbsoluteTimeGetCurrent() + 10;
		
		// wait for the started notification
		while (aqr.queueStartStopTime == 0.) {
			CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.010, FALSE);
			if (CFAbsoluteTimeGetCurrent() >= waitUntil) {
				fprintf(stderr, "Timeout waiting for the queue's IsRunning notification\n");
				goto cleanup;
			}
		}
		printf("Recording...\n");
		CFAbsoluteTime stopTime = aqr.queueStartStopTime + seconds;
		CFAbsoluteTime now = CFAbsoluteTimeGetCurrent();
		CFRunLoopRunInMode(kCFRunLoopDefaultMode, stopTime - now, FALSE);
	} else {
		// start the queue
		aqr.running = TRUE;
		CheckError(AudioQueueStart(aqr.queue, NULL), "AudioQueueStart failed");
		
		// and wait
		printf("Recording, press <return> to stop:\n");
		getchar();
	}
#endif
	
	aqr.running = YES;
	rc = AudioQueueStart(aqr.queue, NULL);
	if(rc)
	{
		NSLog(@"AudioQueueStart failed. rc = %d", rc);
		goto cleanup;
	}
	
	rc = MPWaitOnSemaphore(detectorThreadRunningSempahore, kDurationForever);
	if(rc)
		NSLog(@"detectorThread MPWaitOnSemaphore failed. Result is %d", rc);
	
	// end recording
	NSLog(@"Done recording");
	
	aqr.running = FALSE;
	
	rc = AudioQueueStop(aqr.queue, TRUE);
	if(rc)
		NSLog(@"AudioQueueStop failed");
	
	
cleanup:
	if(aqr.queue)
		AudioQueueDispose(aqr.queue, TRUE);
		
	mDetectorThreadIsFinished = YES;
	[pool release];
}

@end

// ____________________________________________________________________________________
// Determine the size, in bytes, of a buffer necessary to represent the supplied number
// of seconds of audio data.
static int MyComputeRecordBufferSize(const AudioStreamBasicDescription *format, AudioQueueRef queue, float seconds)
{
	int packets, frames, bytes;
	
	frames = (int)ceil(seconds * format->mSampleRate);
	
	if (format->mBytesPerFrame > 0)
		bytes = frames * format->mBytesPerFrame;
	else {
		UInt32 maxPacketSize;
		if (format->mBytesPerPacket > 0)
			maxPacketSize = format->mBytesPerPacket;	// constant packet size
		else
		{
			assert(0);
			NSLog(@"MyComputeRecordBufferSize - Shouldn't get here.");
			return 30000;
		}
		if (format->mFramesPerPacket > 0)
			packets = frames / format->mFramesPerPacket;
		else
			packets = frames;	// worst-case scenario: 1 frame in a packet
		if (packets == 0)		// sanity check
			packets = 1;
		bytes = packets * maxPacketSize;
	}
	return bytes;
}

// ____________________________________________________________________________________
// AudioQueue callback function, called when an input buffers has been filled.
static void MyInputBufferHandler(void *                          inUserData,
								 AudioQueueRef                   inAQ,
								 AudioQueueBufferRef             inBuffer,
								 const AudioTimeStamp *          inStartTime,
								 UInt32							inNumPackets,
								 const AudioStreamPacketDescription *inPacketDesc)
{
	MyRecorder *aqr = (MyRecorder *)inUserData;
	
#if 1
		printf("buf data %p, 0x%x bytes, 0x%x packets\n", inBuffer->mAudioData,
			   (int)inBuffer->mAudioDataByteSize, (int)inNumPackets);
#endif
	
#if 0
	if (inNumPackets > 0) {
		// write packets to file
		CheckError(AudioFileWritePackets(aqr->recordFile, FALSE, inBuffer->mAudioDataByteSize,
										 inPacketDesc, aqr->recordPacket, &inNumPackets, inBuffer->mAudioData),
				   "AudioFileWritePackets failed");
		aqr->recordPacket += inNumPackets;
	}
#endif
	
	// if we're not stopping, re-enqueue the buffer so that it gets filled again
	if (aqr->running)
	{
		OSStatus rc = AudioQueueEnqueueBuffer(inAQ, inBuffer, 0, NULL);
		if(rc)
			NSLog(@"MyInputBufferHandler: AudioQueueEnqueueBuffer failed");
	}
}

// ____________________________________________________________________________________
// get sample rate of the default input device
static OSStatus MyGetDefaultInputDeviceSampleRate(Float64 *outSampleRate)
{
	OSStatus err;
	AudioDeviceID deviceID = 0;
	
	// get the default input device
	AudioObjectPropertyAddress addr;
	UInt32 size;
	addr.mSelector = kAudioHardwarePropertyDefaultInputDevice;
	addr.mScope = kAudioObjectPropertyScopeGlobal;
	addr.mElement = 0;
	size = sizeof(AudioDeviceID);

	err = AudioHardwareServiceGetPropertyData(kAudioObjectSystemObject, &addr, 0, NULL, &size, &deviceID);
	if (err) return err;
	
	// get its sample rate
	addr.mSelector = kAudioDevicePropertyNominalSampleRate;
	addr.mScope = kAudioObjectPropertyScopeGlobal;
	addr.mElement = 0;
	size = sizeof(Float64);
	err = AudioHardwareServiceGetPropertyData(deviceID, &addr, 0, NULL, &size, outSampleRate);
	
	return err;
}

