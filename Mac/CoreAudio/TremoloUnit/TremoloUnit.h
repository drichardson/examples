/*
*	File:		TremoloUnit.h
*	
*	Version:	1.0
* 
*	Created:	3/16/08
*	
*	Copyright:  Copyright © 2008 Zizzam, All Rights Reserved
* 
*	Disclaimer:	IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc. ("Apple") in 
*				consideration of your agreement to the following terms, and your use, installation, modification 
*				or redistribution of this Apple software constitutes acceptance of these terms.  If you do 
*				not agree with these terms, please do not use, install, modify or redistribute this Apple 
*				software.
*
*				In consideration of your agreement to abide by the following terms, and subject to these terms, 
*				Apple grants you a personal, non-exclusive license, under Apple's copyrights in this 
*				original Apple software (the "Apple Software"), to use, reproduce, modify and redistribute the 
*				Apple Software, with or without modifications, in source and/or binary forms; provided that if you 
*				redistribute the Apple Software in its entirety and without modifications, you must retain this 
*				notice and the following text and disclaimers in all such redistributions of the Apple Software. 
*				Neither the name, trademarks, service marks or logos of Apple Computer, Inc. may be used to 
*				endorse or promote products derived from the Apple Software without specific prior written 
*				permission from Apple.  Except as expressly stated in this notice, no other rights or 
*				licenses, express or implied, are granted by Apple herein, including but not limited to any 
*				patent rights that may be infringed by your derivative works or by other works in which the 
*				Apple Software may be incorporated.
*
*				The Apple Software is provided by Apple on an "AS IS" basis.  APPLE MAKES NO WARRANTIES, EXPRESS OR 
*				IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF NON-INFRINGEMENT, MERCHANTABILITY 
*				AND FITNESS FOR A PARTICULAR PURPOSE, REGARDING THE APPLE SOFTWARE OR ITS USE AND OPERATION ALONE 
*				OR IN COMBINATION WITH YOUR PRODUCTS.
*
*				IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL OR CONSEQUENTIAL 
*				DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
*				OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) ARISING IN ANY WAY OUT OF THE USE, 
*				REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION OF THE APPLE SOFTWARE, HOWEVER CAUSED AND WHETHER 
*				UNDER THEORY OF CONTRACT, TORT (INCLUDING NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN 
*				IF APPLE HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*
*/
#include "AUEffectBase.h"
#include "TremoloUnitVersion.h"

#if AU_DEBUG_DISPATCHER
	#include "AUDebugDispatcher.h"
#endif


#ifndef __TremoloUnit_h__
#define __TremoloUnit_h__


#pragma mark ____TremoloUnit Parameters

static CFStringRef kParamName_Tremolo_Freq = CFSTR ("Frequency"); // 1 
static const float kDefaultValue_Tremolo_Freq = 2.0; // 2 
static const float kMinimumValue_Tremolo_Freq = 0.5; // 3 
static const float kMaximumValue_Tremolo_Freq = 20.0; // 4 

static CFStringRef kParamName_Tremolo_Depth = CFSTR ("Depth"); // 5 
static const float kDefaultValue_Tremolo_Depth = 50.0; 
static const float kMinimumValue_Tremolo_Depth = 0.0; 
static const float kMaximumValue_Tremolo_Depth = 100.0; 

static CFStringRef kParamName_Tremolo_Waveform = CFSTR ("Waveform"); // 6 
static const int kSineWave_Tremolo_Waveform = 1; 
static const int kSquareWave_Tremolo_Waveform = 2; 
static const int kDefaultValue_Tremolo_Waveform = kSineWave_Tremolo_Waveform;

// menu item names for the waveform parameter 
static CFStringRef kMenuItem_Tremolo_Sine = CFSTR ("Sine"); // 7 
static CFStringRef kMenuItem_Tremolo_Square = CFSTR ("Square"); // 8 

// parameter identifiers 
enum { // 9 
	kParameter_Frequency = 0, 
	kParameter_Depth = 1, 
	kParameter_Waveform = 2, 
	kNumberOfParameters = 3 
};



#pragma mark ____TremoloUnit Factory Preset Constants 
static const float kParameter_Preset_Frequency_Slow = 2.0; // 1 
static const float kParameter_Preset_Frequency_Fast = 20.0; // 2 
static const float kParameter_Preset_Depth_Slow = 50.0; // 3 
static const float kParameter_Preset_Depth_Fast = 90.0; // 4 
static const float kParameter_Preset_Waveform_Slow = kSineWave_Tremolo_Waveform; 
static const float kParameter_Preset_Waveform_Fast = kSquareWave_Tremolo_Waveform; 

enum { 
	kPreset_Slow = 0, // 7 
	kPreset_Fast = 1, // 8 
	kNumberPresets = 2 // 9 
}; 

static AUPreset kPresets [kNumberPresets] = { // 10 
	{kPreset_Slow, CFSTR ("Slow & Gentle")}, 
	{kPreset_Fast, CFSTR ("Fast & Hard")} 
}; 

static const int kPreset_Default = kPreset_Slow; // 11 


#pragma mark ____TremoloUnit
class TremoloUnit : public AUEffectBase
{
public:
	TremoloUnit(AudioUnit component);
#if AU_DEBUG_DISPATCHER
	virtual ~TremoloUnit () { delete mDebugDispatcher; }
#endif
	
	virtual AUKernelBase *		NewKernel() { return new TremoloUnitKernel(this); }
	
	virtual	ComponentResult		GetParameterValueStrings(AudioUnitScope			inScope,
														 AudioUnitParameterID		inParameterID,
														 CFArrayRef *			outStrings);
    
	virtual	ComponentResult		GetParameterInfo(AudioUnitScope			inScope,
												 AudioUnitParameterID	inParameterID,
												 AudioUnitParameterInfo	&outParameterInfo);
    
	virtual ComponentResult		GetPropertyInfo(AudioUnitPropertyID		inID,
												AudioUnitScope			inScope,
												AudioUnitElement		inElement,
												UInt32 &			outDataSize,
												Boolean	&			outWritable );
	
	virtual ComponentResult		GetProperty(AudioUnitPropertyID inID,
											AudioUnitScope 		inScope,
											AudioUnitElement 		inElement,
											void *			outData);
	
 	virtual	bool				SupportsTail () { return false; }
	
	/*! @method Version */
	virtual ComponentResult	Version() { return kTremoloUnitVersion; }
	
    virtual ComponentResult GetPresets(CFArrayRef *outData) const;
	virtual OSStatus NewFactoryPresetSet(const AUPreset &inNewFactoryPreset);
	
protected:
	class TremoloUnitKernel : public AUKernelBase		// most of the real work happens here
	{
	public:
		TremoloUnitKernel(AUEffectBase *inAudioUnit );
		
		// *Required* overides for the process method for this effect
		// processes one channel of interleaved samples
        virtual void 		Process(	const Float32 	*inSourceP,
										Float32		 	*inDestP,
										UInt32 			inFramesToProcess,
										UInt32			inNumChannels,
										bool			&ioSilence);
		
        virtual void		Reset();
		
		virtual bool SupportsTail () {return true;}
		
	private: //state variables...
		enum {kWaveArraySize = 2000};
		float mSine[kWaveArraySize];
		float mSquare[kWaveArraySize];
		float *waveArrayPointer;
		Float32 mSampleFrequency;
		long mSamplesProcessed;
		enum {sampleLimit = (int)10E6};
		float mCurrentScale;
		float mNextScale;
	};
};

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#endif