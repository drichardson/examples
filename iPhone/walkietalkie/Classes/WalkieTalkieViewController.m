//
//  WalkieTalkieViewController.m
//  WalkieTalkie
//
//  Created by Doug on 1/22/09.
//  Copyright Douglas Richardson 2009. All rights reserved.
//

#import "WalkieTalkieViewController.h"
#include <AudioToolbox/AudioToolbox.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <pthread.h>

#define kWalkieTalkiePort	7987


// 1492 is the 1500 max ethernet payload (i.e. the largest MTU gaurenteed to get passed along by routers) - the 8 byte UDP header.
#define kMaxPacketLength 1492
//#define kMaxPacketLength 2500

#define kPacketHeaderSize (sizeof(uint16_t) + sizeof(uint16_t))

#pragma pack(push, 0)
typedef struct AudioPacket
{
	uint16_t command;
	uint16_t sequenceNumber;
	uint8_t audioData[kMaxPacketLength - kPacketHeaderSize];
} AudioPacket;
#pragma pack(pop)


typedef struct AudioPacketWithTrustedSize
{
	AudioPacket audioPacket;
	int audioDataSize;
} AudioPacketWithTrustedSize;

#define kRecordingNumberBuffers 3

typedef struct AQRecorderState {
    AudioStreamBasicDescription  mDataFormat;
    AudioQueueRef                mQueue;
    AudioQueueBufferRef          mBuffers[kRecordingNumberBuffers];
    AudioFileID                  mAudioFile;
    UInt32                       bufferByteSize;
    bool                         mIsRunning;
	
	WalkieTalkieViewController *controller;
} AQRecorderState;

#define kPlaybackNumberBuffers 3
#define kAudioPacketBufferCount 20

typedef struct AQPlaybackState {
    AudioStreamBasicDescription  mDataFormat;
    AudioQueueRef                mQueue;
    AudioQueueBufferRef          mBuffers[kPlaybackNumberBuffers];
    AudioFileID                  mAudioFile;
    UInt32                       bufferByteSize;
    bool                         mIsRunning;
	
	AudioPacketWithTrustedSize	mAudioPackets[kAudioPacketBufferCount];
	int							mNextReadAudioPacket;
	int							mNextWriteAudioPacket;
	int							mAudioPacketCount;
	pthread_mutex_t				mAudioPacketsMutex;
	
	WalkieTalkieViewController *controller;
} AQPlaybackState;


@interface WalkieTalkieViewController ()

- (void)oneTimeInitialization;

@property int socket;
@property (readonly) const struct sockaddr* multicastAddress;
@property (readwrite) int sequence;
@end


static void DeriveBufferSize (AudioQueueRef                audioQueue,
							  const AudioStreamBasicDescription  *ASBDescription,
							  Float64                      seconds,
							  UInt32                       *outBufferSize);

static void HandleInputBuffer (void                                *aqData,
							   AudioQueueRef                       inAQ,
							   AudioQueueBufferRef                 inBuffer,
							   const AudioTimeStamp                *inStartTime,
							   UInt32                              inNumPackets,
							   const AudioStreamPacketDescription  *inPacketDesc);

static void HandleOutputBuffer (void                *aqData,
								AudioQueueRef       inAQ,
								AudioQueueBufferRef inBuffer);

static void MyInterruptionListener (void     *inClientData,
									UInt32   inInterruptionState);

@implementation WalkieTalkieViewController

@synthesize socket = writeSocket;
@synthesize sequence;

- (const struct sockaddr*)multicastAddress
{
	return (struct sockaddr*)&_saddr;
}


// The designated initializer. Override to perform setup that is required before the view is loaded.
- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    if (self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil])
	{
        // Custom initialization
		[self oneTimeInitialization];
    }
	
    return self;
}

- (void)oneTimeInitialization
{
	if(writeSocket != 0)
		return;
	
	const Float64 kSecondsOfAudio = 0.1;
	
	// initialize the audio session object for this application,
	//		registering the callback that Audio Session Services will invoke 
	//		when there's an interruption
	OSStatus rc;
	
	rc = AudioSessionInitialize (NULL, NULL, MyInterruptionListener, self);
	
	if(rc != noErr)
		NSLog(@"AudioSessionInitialize with %d", rc);
	
	_aqData = calloc(1, sizeof(AQRecorderState));
	_aqData->controller = self;
	
	_aqPlayback = calloc(1, sizeof(AQPlaybackState));
	_aqPlayback->controller = self;
	pthread_mutexattr_t attr;
	pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&_aqPlayback->mAudioPacketsMutex, &attr);
		
	rc = AudioSessionSetActive(true);
	
	if(rc != noErr)
		NSLog(@"AudioSessionSetActive(true) failed with %d", rc);
	
	
	//
	// Setup the recording audio queue
	//
	
	_aqData->mDataFormat.mFormatID         = kAudioFormatLinearPCM; // 2
	//_aqData->mDataFormat.mSampleRate       = 44100.0;               // 3
	_aqData->mDataFormat.mSampleRate       = 11025.0;               // 3
	_aqData->mDataFormat.mChannelsPerFrame = 1;                     // 4
	_aqData->mDataFormat.mBitsPerChannel   = 16;                    // 5
	_aqData->mDataFormat.mBytesPerPacket   =  _aqData->mDataFormat.mBytesPerFrame = _aqData->mDataFormat.mChannelsPerFrame * (_aqData->mDataFormat.mBitsPerChannel / 8);
	_aqData->mDataFormat.mFramesPerPacket  = 1;                     // 7
	
	//AudioFileTypeID fileType             = kAudioFileAIFFType;    // 8
	_aqData->mDataFormat.mFormatFlags = kLinearPCMFormatFlagIsBigEndian | kLinearPCMFormatFlagIsSignedInteger | kLinearPCMFormatFlagIsPacked;	
	
	
	rc = AudioQueueNewInput (                              // 1
							 &_aqData->mDataFormat,                          // 2
							 HandleInputBuffer,                            // 3
							 _aqData,                                      // 4
							 NULL,                                         // 5
							 kCFRunLoopCommonModes,                        // 6
							 0,                                            // 7
							 &_aqData->mQueue                                // 8
							 );
	
	if(rc != noErr)
		NSLog(@"AudioQueueNewInput failed with %d", rc);
	
	
	UInt32 dataFormatSize = sizeof (_aqData->mDataFormat);       // 1
	
	rc = AudioQueueGetProperty (_aqData->mQueue,
								kAudioQueueProperty_StreamDescription,
								// in Mac OS X, instead use
								//    kAudioConverterCurrentInputStreamDescription
								&_aqData->mDataFormat,
								&dataFormatSize);
	
	if(rc != noErr)
		NSLog(@"AudioQueueGetProperty failed with %d", rc);
	
	
	DeriveBufferSize (_aqData->mQueue, &_aqData->mDataFormat, kSecondsOfAudio, &_aqData->bufferByteSize);
	
	
	//
	// Setup the playback audio queue
	//
	_aqPlayback->mDataFormat = _aqData->mDataFormat;
	
	rc = AudioQueueNewOutput(&_aqPlayback->mDataFormat,
							 HandleOutputBuffer,
							 _aqPlayback, 
							 NULL,
							 kCFRunLoopCommonModes,
							 0,
							 &_aqPlayback->mQueue);
	
	if(rc != noErr)
		NSLog(@"AudioQueueNewOutput failed with %d", rc);
	
	
	DeriveBufferSize (_aqPlayback->mQueue, &_aqPlayback->mDataFormat, kSecondsOfAudio, &_aqPlayback->bufferByteSize);	
	
	
	
	//
	// Setup the sockets
	//	
	
	// Bind the socket to our port
	bzero(&_saddr, sizeof(_saddr));
	_saddr.sin_port = htons(kWalkieTalkiePort);
	_saddr.sin_family = AF_INET;
	_saddr.sin_len = sizeof(_saddr);
	inet_pton(AF_INET, "231.60.33.201", &_saddr.sin_addr);
	
	// Setup the read socket
	readSocket = socket(AF_INET, SOCK_DGRAM, 0);
	if(readSocket == - 1)
		NSLog(@"read socket retrned %d", readSocket);
	
	int err = bind(readSocket, (struct sockaddr*)&_saddr, sizeof(_saddr));
	if(err != 0)
		NSLog(@"bind returned %d. %s", err, strerror(errno));
	
	
	// Setup the write socket
	
	writeSocket = socket(AF_INET, SOCK_DGRAM, 0);
	if(writeSocket == -1)
		NSLog(@"socket returned %d", writeSocket);
	
	struct ip_mreq mreq;
	mreq.imr_interface.s_addr = INADDR_ANY;
	mreq.imr_multiaddr = _saddr.sin_addr;
	err = setsockopt(readSocket, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof(mreq));
	if(err != 0)
		NSLog(@"setsockopt(IP_ADD_MEMBERSHIP) failed with %d", err);
	
	// Make the socket multi-cast options
	u_char timeToLive = 255;
	err = setsockopt(writeSocket, IPPROTO_IP, IP_MULTICAST_TTL, &timeToLive, sizeof(timeToLive));
	if(err != 0)
		NSLog(@"setsockopt(IP_MULTICAST_TTL) failed with %d (%s)", err, strerror(errno));
	
	u_char multicastLoopbackEnabled = 0;
	err = setsockopt(writeSocket, IPPROTO_IP, IP_MULTICAST_LOOP, &multicastLoopbackEnabled, sizeof(multicastLoopbackEnabled));
	if(err != 0)
		NSLog(@"setsockopt(IP_MULTICAST_LOOP) failed with %d", err);
	
	//sendto(writeSocket, "Testing", 7, 0, (struct sockaddr*)&_saddr, sizeof(_saddr));
	
	[NSThread detachNewThreadSelector:@selector(readSocketThread:) toTarget:self withObject:nil];
}

- (void)readSocketThread:(id)arg
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	
	int audioBufferPrimeCount = 0;
	
	while(1776)
	{
		NSAutoreleasePool *subpool = [[NSAutoreleasePool alloc] init];
		
		AudioPacket packet;
		
		ssize_t bytesReceived = recvfrom(readSocket, &packet, sizeof(packet), 0, NULL, NULL);
		
		NSLog(@"bytesReceived = %d", bytesReceived);
		
		if(bytesReceived > 0 && bytesReceived <= sizeof(packet))
		{
			packet.command = ntohs(packet.command);
			packet.sequenceNumber = ntohs(packet.sequenceNumber);
			
			//NSLog(@"Got cmd=%u, seq=%u", packet.command, packet.sequenceNumber);
			
			if(packet.command == 1)
			{
				// Enqueue the audio data.
				pthread_mutex_lock(&_aqPlayback->mAudioPacketsMutex);
				bcopy(&packet, &_aqPlayback->mAudioPackets[_aqPlayback->mNextWriteAudioPacket].audioPacket, sizeof(packet));
				_aqPlayback->mAudioPackets[_aqPlayback->mNextReadAudioPacket].audioDataSize = bytesReceived - kPacketHeaderSize;
				//NSLog(@"Putting %d bytes into the packet.", _aqPlayback->mAudioPackets[_aqPlayback->mNextReadAudioPacket].audioDataSize);
				int lastWritten = _aqPlayback->mNextWriteAudioPacket;
				_aqPlayback->mNextWriteAudioPacket = (_aqPlayback->mNextWriteAudioPacket + 1) % kAudioPacketBufferCount;
				_aqPlayback->mAudioPacketCount++;
				
				if(_aqPlayback->mAudioPacketCount > kAudioPacketBufferCount)
				{
					// We caught up with the buffered data. Truncate the old data so new data isn't played followed by older data.
					_aqPlayback->mNextReadAudioPacket = lastWritten;
					_aqPlayback->mAudioPacketCount = 1;
					NSLog(@"Truncating audio because circular queue ran out of space.");
				}
				
				if(!_aqPlayback->mIsRunning)
				{					
					OSStatus rc;
					
					rc = AudioQueueAllocateBuffer(_aqPlayback->mQueue, _aqPlayback->bufferByteSize, &_aqPlayback->mBuffers[audioBufferPrimeCount]);
						
					if(rc != noErr)
						NSLog(@"AudioQueueAllocateBuffer failed with %d", rc);
					
					// Prime the output buffer.
					HandleOutputBuffer(_aqPlayback, _aqPlayback->mQueue, _aqPlayback->mBuffers[audioBufferPrimeCount]);
					
					if(audioBufferPrimeCount >= kPlaybackNumberBuffers)
					{
						audioBufferPrimeCount = 0;
						_aqPlayback->mIsRunning = true;
						[NSThread detachNewThreadSelector:@selector(playbackThread:) toTarget:self withObject:nil];
					}
					else
						audioBufferPrimeCount++;
					
				}
				
				pthread_mutex_unlock(&_aqPlayback->mAudioPacketsMutex);
			}
			
		}
		
		
		[subpool release];
	}
	
	[pool release];
}

- (void)playbackThread:(id)arg
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
		
	OSStatus rc = AudioQueueStart(_aqPlayback->mQueue, NULL);
	if(rc != noErr)
		NSLog(@"AudioQueueStart failed for playback with %d", rc);
	
	do
	{
		CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.25, false);
	} while (_aqPlayback->mIsRunning);
	
	//CFRunLoopRunInMode(kCFRunLoopDefaultMode, 1, false);
		
	[pool release];
}

/*
// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView {
	[self oneTimeInitialization];
}
*/



// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad {
	
	[self oneTimeInitialization];
	
    [super viewDidLoad];
}


/*
// Override to allow orientations other than the default portrait orientation.
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    // Return YES for supported orientations
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}
*/


- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning]; // Releases the view if it doesn't have a superview
    // Release anything that's not essential, such as cached data
}


- (void)dealloc
{
	if(_aqData)
		free(_aqData);
	
	
	pthread_mutex_destroy(&_aqPlayback->mAudioPacketsMutex);
	
	if(_aqPlayback)
		free(_aqPlayback);
	
    [super dealloc];
}


#pragma mark --- IBActions ---

- (IBAction)talkDown:(id)sender
{
	NSLog(@"talkDown");
	
	OSStatus rc;
	
	for (int i = 0; i < kRecordingNumberBuffers; ++i)
	{
		rc = AudioQueueAllocateBuffer (_aqData->mQueue, _aqData->bufferByteSize, &_aqData->mBuffers[i]);
		if(rc != noErr)
			NSLog(@"AudioQueueAllocateBuffer #%d failed with %d", i, rc);
		
		rc = AudioQueueEnqueueBuffer (_aqData->mQueue, _aqData->mBuffers[i], 0, NULL);
		if(rc != noErr)
			NSLog(@"AudioQueueEnqueueBuffer #%d failed with %d", i, rc);
	}
	
	_aqData->mIsRunning = true;
	
	
	rc = AudioQueueStart (_aqData->mQueue, NULL);
	
	if(rc != noErr)
		NSLog(@"AudioQueueStart failed with %d", rc);
}

- (IBAction)talkUp:(id)sender
{
	NSLog(@"talkUp");
	
#if 1
	OSStatus rc;
	
	
	// Wait, on user interface thread, until user stops the recording
	rc = AudioQueueStop(_aqData->mQueue, true);
	if(rc != noErr)
		NSLog(@"AudioQueueStop failed with %d.", rc);
	
	_aqData->mIsRunning = false;
	
#endif
}

@end

static void
HandleInputBuffer (void                                *aqData,
				   AudioQueueRef                       inAQ,
				   AudioQueueBufferRef                 inBuffer,
				   const AudioTimeStamp                *inStartTime,
				   UInt32                              inNumPackets,
				   const AudioStreamPacketDescription  *inPacketDesc)
{
	//NSLog(@"HandleInputBuffer called");
	
	AQRecorderState *myState = (AQRecorderState*)aqData;
	
	const struct sockaddr *saddr = myState->controller.multicastAddress;
	AudioPacket packet;
	int sequence = myState->controller.sequence;
	packet.command = htons(1);
	
	int socket = myState->controller.socket;
	
	uint8_t *end = inBuffer->mAudioData + inBuffer->mAudioDataByteSize;
	const size_t kMaxAudioLength = sizeof(packet.audioData);
	for(uint8_t *cur = inBuffer->mAudioData; cur < end; cur += kMaxAudioLength)
	{
		packet.sequenceNumber = htons(sequence);
		sequence++;
		
		
		int bytesToCopy = end - cur >= kMaxAudioLength ? kMaxAudioLength : end - cur;
		
		if(bytesToCopy <= 0)
		{
			NSLog(@"Bytes to copy is unexpected value of %d", bytesToCopy);
			break;
		}
		
		bcopy(cur, packet.audioData, bytesToCopy);
		
		int packetLen = sizeof(packet.command) + sizeof(packet.sequenceNumber) + bytesToCopy;
		
		int rc = sendto(socket, &packet, packetLen, 0, saddr, saddr->sa_len);
		
		if(rc != packetLen)
			NSLog(@"Error sending %d bytes. Only sent %d", packetLen, rc);
		
		NSLog(@"Sent %d bytes.", rc);
	}
	
	myState->controller.sequence = sequence;
	
	AudioQueueEnqueueBuffer(myState->mQueue, inBuffer, 0, NULL);
}


static void
HandleOutputBuffer (void                *aqData,
					AudioQueueRef       inAQ,
					AudioQueueBufferRef inBuffer)
{
    AQPlaybackState *pAqData = (AQPlaybackState *) aqData;
	
	BOOL gotPacket = NO;
	OSStatus rc;
	BOOL stopQueue = NO;
	
	pthread_mutex_lock(&pAqData->mAudioPacketsMutex);
	
	int packetCountDebug = pAqData->mAudioPacketCount;
	int packetNumberDebug = pAqData->mNextReadAudioPacket;
	int packetSizeDebug = pAqData->mAudioPackets[pAqData->mNextReadAudioPacket].audioDataSize;
	
	if(pAqData->mAudioPacketCount > 0)
	{
		AudioPacketWithTrustedSize *packet = &pAqData->mAudioPackets[pAqData->mNextReadAudioPacket];
		bcopy(packet->audioPacket.audioData, inBuffer->mAudioData, packet->audioDataSize);
		inBuffer->mAudioDataByteSize = packet->audioDataSize;
		
		pAqData->mNextReadAudioPacket = (pAqData->mNextReadAudioPacket + 1) % kAudioPacketBufferCount;
		pAqData->mAudioPacketCount--;
		gotPacket = YES;
	}
	else
	{
		NSLog(@"Stopping playback queue because I ran out of packets.");
		stopQueue = YES;
	}
	pthread_mutex_unlock(&pAqData->mAudioPacketsMutex);
	
	if(gotPacket)
	{
		NSLog(@"Enqueuing sound packet. Count is %d. Packet number %d of size %d", packetCountDebug, packetNumberDebug, packetSizeDebug);
		rc = AudioQueueEnqueueBuffer(pAqData->mQueue, inBuffer, 0, NULL);
		if(rc != noErr)
		{
			NSLog(@"AudioQueueEnqueueBuffer failed for playback with %d.", rc);
			stopQueue = YES;
		}
	}
	
	if(stopQueue)
	{
		pthread_mutex_lock(&pAqData->mAudioPacketsMutex);
		
		rc = AudioQueueStop(pAqData->mQueue, true);
		
		if(rc != noErr)
			NSLog(@"AudioQueueStop failed with %d", rc);
		
		pAqData->mIsRunning = false;
		
		pthread_mutex_unlock(&pAqData->mAudioPacketsMutex);
	}
}


static void 
MyInterruptionListener (void     *inClientData,
						UInt32   inInterruptionState)
{
}

static void
DeriveBufferSize (AudioQueueRef                audioQueue,
				  const AudioStreamBasicDescription  *ASBDescription,
				  Float64                      seconds,
				  UInt32                       *outBufferSize)
{
    static const int maxBufferSize = 0x50000;                 // 5
	
    int maxPacketSize = ASBDescription->mBytesPerPacket;       // 6
    if (maxPacketSize == 0) {                                 // 7
        UInt32 maxVBRPacketSize = sizeof(maxPacketSize);
        AudioQueueGetProperty (
							   audioQueue,
							   kAudioQueueProperty_MaximumOutputPacketSize,
							   // in Mac OS X v10.5, instead use
							   //   kAudioConverterPropertyMaximumOutputPacketSize
							   &maxPacketSize,
							   &maxVBRPacketSize
							   );
    }
	
    Float64 numBytesForTime = ASBDescription->mSampleRate * maxPacketSize * seconds;
    *outBufferSize = (uint32_t) (numBytesForTime < maxBufferSize ? numBytesForTime : maxBufferSize);
}
