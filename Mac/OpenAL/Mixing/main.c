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

static bool LoadWAVIntoBuffer(const char* filename, ALuint buffer)
{
	ALenum format = 0;
	ALvoid* data = NULL;
	ALsizei size = 0, freq = 0;
	Float64 estimatedDurationInSeconds = 0;
	
	if ( !LoadWAVFile( filename, &format, &data, &size, &freq, &estimatedDurationInSeconds ) )
	{
		fprintf( stderr, "Couldn't load %s\n", filename );
		return false;
	}
	
	// copy the wav into AL buffer 0
	alBufferData( buffer, format, data, size, freq );
	free(data);
	ALenum error = alGetError();
	if ( error != AL_NO_ERROR )
	{
		fprintf( stderr, "Error copying helloworld wav. %d\n", error );
		return false;
	}
	
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
	
	ALuint buffers[3];
	alGenBuffers( 3, buffers );
	
	ALenum error = alGetError();
	
	if ( error != AL_NO_ERROR )
	{
		fprintf( stderr, "Couldn't generate buffers. %d\n", error );
		exit(1);
	}
	
	if ( !LoadWAVIntoBuffer( "music.wav", buffers[0] ) )
		exit(1);
	
	if ( !LoadWAVIntoBuffer( "gunshot.wav", buffers[1] ) )
		exit(1);
	
	if ( !LoadWAVIntoBuffer( "meep-meep.wav", buffers[2] ) )
		exit(1);
	
	//
	// Generate sources
	//
	ALuint sources[3];
	alGenSources( 3, sources );
	error = alGetError();
	if ( error != AL_NO_ERROR )
	{
		fprintf( stderr, "Error generating sources. %d\n", error );
		exit(1);
	}
	
	//
	// Attach buffer to source
	//
	for(int i = 0; i < 3; ++i)
	{
		alSourcei( sources[i], AL_BUFFER, buffers[i] );
		error = alGetError();
		if ( error != AL_NO_ERROR )
		{
			fprintf( stderr, "Error attaching buffer %d to source. %d\n", i, error );
			exit(1);
		}
	}
	
	// Start playing the background music.
	alSourcef( sources[0], AL_GAIN, 0.5 ); // -6db for background music
	alSourcei( sources[0], AL_LOOPING, AL_TRUE );
	alSourcePlay( sources[0] );
	
	int c;
	bool printPrompt = true;
	do
	{
		if ( printPrompt )
		{
			printf( "> " );
			fflush( stdout );
		}
		
		c = getchar();
		
		switch ( c )
		{
			case 'g':
				alSourcePlay( sources[1] );
				printPrompt = false;
				break;
			case 'm':
				alSourcePlay( sources[2] );
				printPrompt = false;
				break;
			case 'q':
			case EOF:
				break;
			case '\n':
				printPrompt = true;
				break;
			default:
				printf( "Unhandled command. Expected g or m\n" );
				break;
		}
	} while ( c != EOF && c != 'q' );
	
	alcMakeContextCurrent( NULL );
	alcDestroyContext( context );
	alcCloseDevice( device );
	
	puts( "OK" );
	
    return 0;
}
