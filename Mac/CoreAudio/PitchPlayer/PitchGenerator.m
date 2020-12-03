//
//  PitchGenerator.m
//  PitchPlayer
//
//  Created by Doug on 3/19/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "PitchGenerator.h"

#include <AudioToolbox/AudioQueue.h>
#include <AudioToolbox/AudioFile.h>
#include <math.h>
#include <limits.h>

#define kNumberBuffers ((int)3)

static void AQPitchGeneratorBufferCallback(void * inUserData, AudioQueueRef inAQ, AudioQueueBufferRef inCompleteAQBuffer);
static void CalculateBytesForTime (AudioStreamBasicDescription * inDesc, UInt32 inMaxPacketSize, Float64 inSeconds, UInt32 *outBufferSize, UInt32 *outNumPackets);


struct AQPitchGeneratorInfo {
	AudioStreamBasicDescription		mDataFormat;
	AudioQueueRef					mQueue;
	AudioQueueBufferRef				mBuffers[kNumberBuffers];
	SInt64							mCurrentPacket;
	UInt32							mNumPacketsToRead;
	AudioStreamPacketDescription *	mPacketDescs;
	BOOL*							mDone;
	
	double							mPitchFrequencyInHz;
	UInt32							mCurrentSampleInThisSecond;
	UInt32							mSamplesPerSecondInHz; // Stored in an integer to show it needs to be an int (as opposed to the Float64 in the stream description).
	//double							mWaveSpeed; // Holds the desired frequency multipled by 2pi. This is pre-calculated for efficiency.
};

typedef struct AQPitchGeneratorInfo AQPitchGeneratorInfo;

@interface PitchGenerator (private)
-(void)pitchThread:(id)unused;
@end

@implementation PitchGenerator

@synthesize frequencyInHz;

-(id)init
{
	self = [super init];
	if(self)
	{
		mThreadFinished = YES;
	}
	return self;
}

-(void)startPlaying
{
	if(!mThreadFinished)
		return;
	
	NSLog(@"startPlaying: %@ Hz", frequencyInHz);
	mDone = NO;
	mThreadFinished = NO;
	[NSThread detachNewThreadSelector:@selector(pitchThread:) toTarget:self withObject:nil];
}

-(void)stopPlaying
{
	NSLog(@"stopPlaying");
	mDone = YES;
}

#if 0
enum
{
	kAudioFormatFlagIsFloat                     = (1L << 0),
	kAudioFormatFlagIsBigEndian                 = (1L << 1),
	kAudioFormatFlagIsSignedInteger             = (1L << 2),
	kAudioFormatFlagIsPacked                    = (1L << 3),
	kAudioFormatFlagIsAlignedHigh               = (1L << 4),
	kAudioFormatFlagIsNonInterleaved            = (1L << 5),
	kAudioFormatFlagIsNonMixable                = (1L << 6),
	kAudioFormatFlagsAreAllClear                = (1L << 31),
	
	kLinearPCMFormatFlagIsFloat                 = kAudioFormatFlagIsFloat,
	kLinearPCMFormatFlagIsBigEndian             = kAudioFormatFlagIsBigEndian,
	kLinearPCMFormatFlagIsSignedInteger         = kAudioFormatFlagIsSignedInteger,
	kLinearPCMFormatFlagIsPacked                = kAudioFormatFlagIsPacked,
	kLinearPCMFormatFlagIsAlignedHigh           = kAudioFormatFlagIsAlignedHigh,
	kLinearPCMFormatFlagIsNonInterleaved        = kAudioFormatFlagIsNonInterleaved,
	kLinearPCMFormatFlagIsNonMixable            = kAudioFormatFlagIsNonMixable,
	kLinearPCMFormatFlagsAreAllClear            = kAudioFormatFlagsAreAllClear,
	
	kAppleLosslessFormatFlag_16BitSourceData    = 1,
	kAppleLosslessFormatFlag_20BitSourceData    = 2,
	kAppleLosslessFormatFlag_24BitSourceData    = 3,
	kAppleLosslessFormatFlag_32BitSourceData    = 4
};
#endif

-(void)pitchThread:(id)unused
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	
	Float32 volume = 0.5;
	OSStatus result;
	
#if 0
	// See Apple Core Audio Format Specification 1.0 for information on PCM formats, under Core Audio Format Specification -> Required Chunks.
	mSampleRate = 44100, 
    mFormatID = 1819304813, 
    mFormatFlags = 12, 
    mBytesPerPacket = 4, 
    mFramesPerPacket = 1, 
    mBytesPerFrame = 4, 
    mChannelsPerFrame = 2, 
    mBitsPerChannel = 16, 
    mReserved = 0
#endif
	
	AQPitchGeneratorInfo myInfo;
	
	myInfo.mDone = &mDone;
	myInfo.mCurrentSampleInThisSecond = 0;
	myInfo.mSamplesPerSecondInHz = 44100;
	myInfo.mPitchFrequencyInHz = [frequencyInHz doubleValue];
	
	
	myInfo.mDataFormat.mSampleRate = myInfo.mSamplesPerSecondInHz; // PCM supports any sample rate.
	myInfo.mDataFormat.mFormatID = kAudioFormatLinearPCM;
	//myInfo.mDataFormat.mFormatFlags = kLinearPCMFormatFlagIsFloat;
	myInfo.mDataFormat.mFormatFlags = 0; //kAudioFormatFlagIsPacked;
	myInfo.mDataFormat.mFormatFlags = kAudioFormatFlagIsSignedInteger | kAudioFormatFlagIsPacked;
	
#if LITTLE_ENDIAN
#elif BIG_ENDIAN
	//myInfo.mDataFormat.mFormatFlags |= kLinearPCMFormatFlagIsBigEndian;
#elif PDP_ENDIAN
#error Middle endian not supported.
#endif
	
	myInfo.mDataFormat.mFramesPerPacket = 1; // By definition for PCM
	myInfo.mDataFormat.mBytesPerFrame = 4;
	myInfo.mDataFormat.mChannelsPerFrame = 2;
	//myInfo.mDataFormat.mBitsPerChannel = 32; // 32-bit floating point
	myInfo.mDataFormat.mBitsPerChannel = 16;
	myInfo.mDataFormat.mBytesPerPacket = myInfo.mDataFormat.mBytesPerFrame; // Equal to the number of bytes per frame since frames per packet is 1.
	myInfo.mDataFormat.mReserved = 0;
	
	NSLog(@"File format: %c%c%c%c", myInfo.mDataFormat.mFormatID >> 24, myInfo.mDataFormat.mFormatID >> 16, myInfo.mDataFormat.mFormatID >> 8, myInfo.mDataFormat.mFormatID);
	
	result = AudioQueueNewOutput(&myInfo.mDataFormat,
								 AQPitchGeneratorBufferCallback,
								 &myInfo,
								 CFRunLoopGetCurrent(),
								 kCFRunLoopCommonModes,
								 0,
								 &myInfo.mQueue);
	if(result)
	{
		NSLog(@"AudioQueueNew failed: %d, %d", result, kAudioQueueErr_InvalidBuffer);
		goto bail;
	}
	
	UInt32 bufferByteSize;
	
	// we need to calculate how many packets we read at a time, and how big a buffer we need
	// we base this on the size of the packets in the file and an approximate duration for each buffer

	// adjust buffer size to represent about a half second of audio based on this format
	CalculateBytesForTime (&myInfo.mDataFormat, myInfo.mDataFormat.mBytesPerPacket, 0.5/*seconds*/, &bufferByteSize, &myInfo.mNumPacketsToRead);
	myInfo.mPacketDescs = NULL; // we don't provide packet descriptions for constant bit rate formats (like linear PCM)		
	NSLog(@"Buffer Byte Size: %d, Num Packets to Read: %d", (int)bufferByteSize, (int)myInfo.mNumPacketsToRead);
		
	// prime the queue with some data before starting
	myInfo.mCurrentPacket = 0;
	int i;
	for (i = 0; i < kNumberBuffers; ++i) {
		result = AudioQueueAllocateBuffer(myInfo.mQueue, bufferByteSize, &myInfo.mBuffers[i]);
		if(result)
		{
			NSLog(@"AudioQueueAllocateBuffer failed");
			goto bail;
		}
		
		AQPitchGeneratorBufferCallback(&myInfo, myInfo.mQueue, myInfo.mBuffers[i]);
	}
	
	// set the volume of the queue
	AudioQueueSetParameter(myInfo.mQueue, kAudioQueueParam_Volume, volume);
	
	// lets start playing now - stop is called in the AQTestBufferCallback when there's
	// no more to read from the file
	result = AudioQueueStart(myInfo.mQueue, NULL);
	if(result)
	{
		NSLog(@"AudioQueueStart failed");
		goto bail;
	}
	
	do {
		NSLog(@"Running run loop...");
		CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.25, false);
	} while (!mDone);
	
	CFRunLoopRunInMode(kCFRunLoopDefaultMode, 1, false);
	
	result = AudioQueueDispose(myInfo.mQueue, true);
	if(result)
	{
		NSLog(@"AudioQueueDispose(true) failed");
		goto bail;
	}
		
	free(myInfo.mPacketDescs);
	mThreadFinished = YES;

bail:

	[pool release];
}

@end

static void
AQPitchGeneratorBufferCallback(void * inUserData,
							   AudioQueueRef inAQ,
							   AudioQueueBufferRef inCompleteAQBuffer)
{
	AQPitchGeneratorInfo * myInfo = (AQPitchGeneratorInfo *)inUserData;
	if (*myInfo->mDone) return;
	
	UInt32 nPackets = myInfo->mNumPacketsToRead;
	NSLog(@"Reading samples: %d", myInfo->mCurrentSampleInThisSecond);
	// NOTE: Perhaps a better way to do this is to precompute 1 second of the pitch and then read for the precomputed buffer.
	
	// Going to compute y(t) = (sin(f*2*pi*t - pi * 0.5) + 1) * 0.5, where f is the pitch frequency and the aplitude is between 0 and 1.
	// For each pitch to generate:
	//   f*2*pi is constant per pitch and will be called mWaveSpeed in inUserData.
	//   pi * 0.5 is constant all the time and will be called kWaveShiftX. This makes the sound start at 0 amplitude. TODO: Is this necessary?
	//   1 is constant all the time and will be called kWaveShiftY. This makes the amplitude always positive. TODO: Is this necessary?
	//   0.5 is constant all the time and will be called attenuation. TODO: Is this necessary?
	//const double kWaveShiftX = M_PI_2; // pi/2
	//const double kAttenuation = 0.5;
	const SInt16 dynamicRangeMultiplier = SHRT_MAX; // Take advantage of the full dynamic range of 16 bits.
	//const double kAttenuation = 10;
	const double waveSpeed = myInfo->mPitchFrequencyInHz * 2.0 * M_PI;
	double recipocolOfSamplesPerSecond = 1.0 / (double)myInfo->mSamplesPerSecondInHz;
	
	UInt32 *outSamples = inCompleteAQBuffer->mAudioData;
	inCompleteAQBuffer->mAudioDataByteSize = nPackets * myInfo->mDataFormat.mBytesPerPacket;
	long i;
	for(i = 0; i < nPackets; ++i)
	{
		if(*myInfo->mDone)
		{
			NSLog(@"IMMEDIATE STOP");
			AudioQueueStop(myInfo->mQueue, true);
			return;
		}
		// theta needs to go from 0 to 2pi in 1 second.
		// The following should be changed to use incrementing, pre-calculate as much as possible, or use the reciprocol of samples per second with multiplication.
		// The division and all the multiplications do not need to be done in this tight loop.
		double theta = ((double)myInfo->mCurrentSampleInThisSecond) * recipocolOfSamplesPerSecond;
		SInt16 rightChannelValue = ((sin(waveSpeed * theta)) * dynamicRangeMultiplier);
		SInt16 leftChannelValue = rightChannelValue;
		outSamples[i] = rightChannelValue << 16 | leftChannelValue;
		myInfo->mCurrentSampleInThisSecond++; // At 44.1kHz, this will roll over about every 25 hours, which may result in a clicking sound.
	}
	
	AudioQueueEnqueueBuffer(inAQ, inCompleteAQBuffer, 0, NULL);		
}

#if 1
// we only use time here as a guideline
// we're really trying to get somewhere between 16K and 64K buffers, but not allocate too much if we don't need it
static void
CalculateBytesForTime (AudioStreamBasicDescription * inDesc,
					   UInt32 inMaxPacketSize,
					   Float64 inSeconds,
					   UInt32 *outBufferSize,
					   UInt32 *outNumPackets)
{
	static const int maxBufferSize = 0x10000; // limit size to 64K
	static const int minBufferSize = 0x4000; // limit size to 16K
	
	if (inDesc->mFramesPerPacket) {
		Float64 numPacketsForTime = inDesc->mSampleRate / inDesc->mFramesPerPacket * inSeconds;
		*outBufferSize = numPacketsForTime * inMaxPacketSize;
	} else {
		// if frames per packet is zero, then the codec has no predictable packet == time
		// so we can't tailor this (we don't know how many Packets represent a time period
		// we'll just return a default buffer size
		*outBufferSize = maxBufferSize > inMaxPacketSize ? maxBufferSize : inMaxPacketSize;
	}
	
	// we're going to limit our size to our default
	if (*outBufferSize > maxBufferSize && *outBufferSize > inMaxPacketSize)
		*outBufferSize = maxBufferSize;
	else {
		// also make sure we're not too small - we don't want to go the disk for too small chunks
		if (*outBufferSize < minBufferSize)
			*outBufferSize = minBufferSize;
	}
	*outNumPackets = *outBufferSize / inMaxPacketSize;
}
#endif