#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <OpenAL/al.h>
#include <OpenAL/alc.h>
#include <AudioToolbox/AudioToolbox.h>

static bool LoadWAVFile(const char* filename, ALenum* format, ALvoid** data, ALsizei* size, ALsizei* freq, Float64* estimatedDurationOut)
{
	CFStringRef filenameStr = CFStringCreateWithCString( NULL, filename, kCFStringEncodingUTF8 );
	CFURLRef url = CFURLCreateWithFileSystemPath( NULL, filenameStr, kCFURLPOSIXPathStyle, false );
	CFRelease( filenameStr );
	
	AudioFileID audioFile;
	OSStatus error = AudioFileOpenURL( url, kAudioFileReadPermission, kAudioFileWAVEType, &audioFile );
	CFRelease( url );
	
	if ( error != noErr )
	{
		fprintf( stderr, "Error opening audio file. %d\n", error );
		return false;
	}
	
	AudioStreamBasicDescription basicDescription;
	UInt32 propertySize = sizeof(basicDescription);
	error = AudioFileGetProperty( audioFile, kAudioFilePropertyDataFormat, &propertySize, &basicDescription );
	
	if ( error != noErr )
	{
		fprintf( stderr, "Error reading audio file basic description. %d\n", error );
		AudioFileClose( audioFile );
		return false;
	}
	
	if ( basicDescription.mFormatID != kAudioFormatLinearPCM )
	{
		// Need PCM for Open AL. WAVs are (I believe) by definition PCM, so this check isn't necessary. It's just here
		// in case I ever use this with another audio format.
		fprintf( stderr, "Audio file is not linear-PCM. %d\n", basicDescription.mFormatID );
		AudioFileClose( audioFile );
		return false;
	}
	
	UInt64 audioDataByteCount = 0;
	propertySize = sizeof(audioDataByteCount);
	error = AudioFileGetProperty( audioFile, kAudioFilePropertyAudioDataByteCount, &propertySize, &audioDataByteCount );
	if ( error != noErr )
	{
		fprintf( stderr, "Error reading audio file byte count. %d\n", error );
		AudioFileClose( audioFile );
		return false;
	}
	
	Float64 estimatedDuration = 0;
	propertySize = sizeof(estimatedDuration);
	error = AudioFileGetProperty( audioFile, kAudioFilePropertyEstimatedDuration, &propertySize, &estimatedDuration );
	if ( error != noErr )
	{
		fprintf( stderr, "Error reading estimated duration of audio file. %d\n", error );
		AudioFileClose( audioFile );
		return false;
	}
	
	ALenum alFormat = 0;
	
	if ( basicDescription.mChannelsPerFrame == 1 )
	{
		if ( basicDescription.mBitsPerChannel == 8 )
			alFormat = AL_FORMAT_MONO8;
		else if ( basicDescription.mBitsPerChannel == 16 )
			alFormat = AL_FORMAT_MONO16;
		else
		{
			fprintf( stderr, "Expected 8 or 16 bits for the mono channel but got %d\n", basicDescription.mBitsPerChannel );
			AudioFileClose( audioFile );
			return false;
		}
		
	}
	else if ( basicDescription.mChannelsPerFrame == 2 )
	{
		if ( basicDescription.mBitsPerChannel == 8 )
			alFormat = AL_FORMAT_STEREO8;
		else if ( basicDescription.mBitsPerChannel == 16 )
			alFormat = AL_FORMAT_STEREO16;
		else
		{
			fprintf( stderr, "Expected 8 or 16 bits per channel but got %d\n", basicDescription.mBitsPerChannel );
			AudioFileClose( audioFile );
			return false;
		}
	}
	else
	{
		fprintf( stderr, "Expected 1 or 2 channels in audio file but got %d\n", basicDescription.mChannelsPerFrame );
		AudioFileClose( audioFile );
		return false;
	}
	
	UInt32 numBytesToRead = audioDataByteCount;
	void* buffer = malloc( numBytesToRead );
	
	if ( buffer == NULL )
	{
		fprintf( stderr, "Error allocating buffer for audio data of size %u\n", numBytesToRead );
		return false;
	}
	
	error = AudioFileReadBytes( audioFile, false, 0, &numBytesToRead, buffer );
	AudioFileClose( audioFile );
	
	if ( error != noErr )
	{
		fprintf( stderr, "Error reading audio bytes. %d\n", error );
		free(buffer);
		return false;
	}
	
	if ( numBytesToRead != audioDataByteCount )
	{
		fprintf( stderr, "Tried to read %lld bytes from the audio file but only got %d bytes\n", audioDataByteCount, numBytesToRead );
		free(buffer);
		return false;
	}
	
	*freq = basicDescription.mSampleRate;
	*size = audioDataByteCount;
	*format = alFormat;
	*data = buffer;
	*estimatedDurationOut = estimatedDuration;
	
	return true;
}

int main (int argc, const char * argv[]) {

	//
	// Initialization
	//
	ALCdevice* device = alcOpenDevice( NULL );
	
	if ( device == NULL )
	{
		fputs( "Couldn't open device", stderr );
		exit(1);
	}
	
	ALCcontext* context = alcCreateContext( device, NULL );
	
	if ( context == NULL )
	{
		fputs( "Error creating context", stderr );
		exit(1);
	}
	
	alcMakeContextCurrent( context );
	
	
	//
	// Generate buffers
	//
	alGetError(); // clear error code
	
	ALuint buffer;
	alGenBuffers( 1, &buffer );
	
	ALenum error = alGetError();
	
	if ( error != AL_NO_ERROR )
	{
		fprintf( stderr, "Couldn't generate buffer. %d\n", error );
		exit(1);
	}
	
	ALenum format = 0;
	ALvoid* data = NULL;
	ALsizei size = 0, freq = 0;
	Float64 estimatedDurationInSeconds = 0;
	
	if ( !LoadWAVFile( "helloworld.wav", &format, &data, &size, &freq, &estimatedDurationInSeconds ) )
	{
		fputs( "Wouldn't load helloworld.wav", stderr );
		exit(1);
	}
	
	// copy the wav into AL buffer 0
	alBufferData( buffer, format, data, size, freq );
	free(data);
	error = alGetError();
	if ( error != AL_NO_ERROR )
	{
		fprintf( stderr, "Error copying helloworld wav. %d\n", error );
		exit(1);
	}
	
	//
	// Generate sources
	//
	ALuint source = 0;
	alGenSources( 1, &source );
	error = alGetError();
	if ( error != AL_NO_ERROR )
	{
		fprintf( stderr, "Error generating source. %d\n", error );
		exit(1);
	}
	
	//
	// Attach buffer to source
	//
	alSourcei( source, AL_BUFFER, buffer );
	error = alGetError();
	if ( error != AL_NO_ERROR )
	{
		fprintf( stderr, "Error attaching buffer to source. %d\n", error );
		exit(1);
	}
	
	alSourcePlay( source );
	
	int sleepDuration = ceil( estimatedDurationInSeconds );
	printf( "sleeping for %d seconds while the file plays\n", sleepDuration );
	sleep(sleepDuration);
	
	alcMakeContextCurrent( NULL );
	alcDestroyContext( context );
	alcCloseDevice( device );
	
	puts( "OK" );
	
    return 0;
}
