/*
*	File:		TremoloUnit.cpp
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
/*=============================================================================
	TremoloUnit.cpp
	
=============================================================================*/
#include "TremoloUnit.h"


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

COMPONENT_ENTRY(TremoloUnit)


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//	TremoloUnit::TremoloUnit
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TremoloUnit::TremoloUnit(AudioUnit component)
	: AUEffectBase(component)
{
	CreateElements();
	Globals()->UseIndexedParameters(kNumberOfParameters);
	SetAFactoryPresetAsCurrent(kPresets[kPreset_Default]);
	
	SetParameter(kParameter_Frequency, kDefaultValue_Tremolo_Freq);
	SetParameter(kParameter_Depth, kDefaultValue_Tremolo_Depth);
	SetParameter(kParameter_Waveform, kDefaultValue_Tremolo_Waveform);
        
#if AU_DEBUG_DISPATCHER
	mDebugDispatcher = new AUDebugDispatcher (this);
#endif
	
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//	TremoloUnit::GetParameterValueStrings
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ComponentResult		TremoloUnit::GetParameterValueStrings(AudioUnitScope		inScope,
                                                                AudioUnitParameterID	inParameterID,
                                                                CFArrayRef *		outStrings)
{
	if((inScope == kAudioUnitScope_Global) && (inParameterID == kParameter_Waveform))
	{
		if(outStrings == NULL) return noErr;
		
		CFStringRef strings[] = {
			kMenuItem_Tremolo_Sine,
			kMenuItem_Tremolo_Square
		};
		
		*outStrings = CFArrayCreate(NULL, (const void**)strings, sizeof(strings)/sizeof(strings[0]), NULL);
		return noErr;
	}
	
    return kAudioUnitErr_InvalidProperty;
}



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//	TremoloUnit::GetParameterInfo
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ComponentResult		TremoloUnit::GetParameterInfo(AudioUnitScope		inScope,
                                                        AudioUnitParameterID	inParameterID,
                                                        AudioUnitParameterInfo	&outParameterInfo )
{
	ComponentResult result = noErr;

	outParameterInfo.flags = 	kAudioUnitParameterFlag_IsWritable
						|		kAudioUnitParameterFlag_IsReadable;
    
    if (inScope == kAudioUnitScope_Global) {
        switch(inParameterID)
        {
            case kParameter_Frequency:
                AUBase::FillInParameterName (outParameterInfo, kParamName_Tremolo_Freq, false);
                outParameterInfo.unit = kAudioUnitParameterUnit_Hertz;
                outParameterInfo.minValue = kMinimumValue_Tremolo_Freq;
                outParameterInfo.maxValue = kMaximumValue_Tremolo_Freq;
                outParameterInfo.defaultValue = kAudioUnitParameterUnit_Hertz;
				outParameterInfo.flags |= kAudioUnitParameterFlag_DisplayLogarithmic;
                break;
				
			case kParameter_Depth:
				AUBase::FillInParameterName(outParameterInfo, kParamName_Tremolo_Depth, false);
				outParameterInfo.unit = kAudioUnitParameterUnit_Percent;
				outParameterInfo.minValue = kMinimumValue_Tremolo_Depth;
				outParameterInfo.maxValue = kMaximumValue_Tremolo_Depth;
				outParameterInfo.defaultValue = kDefaultValue_Tremolo_Depth;
				break;
				
			case kParameter_Waveform: // 11 
				AUBase::FillInParameterName ( 
											 outParameterInfo, 
											 kParamName_Tremolo_Waveform, 
											 false 
				); 
				outParameterInfo.unit = kAudioUnitParameterUnit_Indexed; 
				outParameterInfo.minValue = kSineWave_Tremolo_Waveform; 
				outParameterInfo.maxValue = kSquareWave_Tremolo_Waveform; 
				outParameterInfo.defaultValue = kDefaultValue_Tremolo_Waveform; 
				break; 
				
            default:
                result = kAudioUnitErr_InvalidParameter;
                break;
            }
	} else {
        result = kAudioUnitErr_InvalidParameter;
    }
    


	return result;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//	TremoloUnit::GetPropertyInfo
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ComponentResult		TremoloUnit::GetPropertyInfo (AudioUnitPropertyID	inID,
                                                        AudioUnitScope		inScope,
                                                        AudioUnitElement	inElement,
                                                        UInt32 &		outDataSize,
                                                        Boolean &		outWritable)
{
	return AUEffectBase::GetPropertyInfo (inID, inScope, inElement, outDataSize, outWritable);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//	TremoloUnit::GetProperty
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ComponentResult		TremoloUnit::GetProperty(	AudioUnitPropertyID inID,
                                                        AudioUnitScope 		inScope,
                                                        AudioUnitElement 	inElement,
                                                        void *			outData )
{
	return AUEffectBase::GetProperty (inID, inScope, inElement, outData);
}

ComponentResult TremoloUnit::GetPresets(CFArrayRef *outData) const
{
	if(outData == NULL) return noErr;
	
	CFMutableArrayRef presetsArray = CFArrayCreateMutable(NULL, kNumberPresets, NULL);
	
	for(int i = 0; i < kNumberPresets; ++i)
		CFArrayAppendValue(presetsArray, &kPresets[i]);
	
	*outData = (CFArrayRef)presetsArray;
	return noErr;
}

OSStatus TremoloUnit::NewFactoryPresetSet(const AUPreset &inNewFactoryPreset)
{
	SInt32 chosenPreset = inNewFactoryPreset.presetNumber;
	
	if(chosenPreset == kPreset_Slow || chosenPreset == kPreset_Fast)
	{
		for(int i = 0; i < kNumberPresets; ++i)
		{
			if(chosenPreset == kPresets[i].presetNumber)
			{
				switch(chosenPreset)
				{
					case kPreset_Slow:
						SetParameter(kParameter_Frequency, kParameter_Preset_Frequency_Slow);
						SetParameter(kParameter_Depth, kParameter_Preset_Depth_Slow);
						SetParameter(kParameter_Waveform, kParameter_Preset_Waveform_Slow);
						break;
						
					case kPreset_Fast:
						SetParameter(kParameter_Frequency, kParameter_Preset_Frequency_Fast);
						SetParameter(kParameter_Depth, kParameter_Preset_Depth_Fast);
						SetParameter(kParameter_Waveform, kParameter_Preset_Waveform_Fast);
						break;
				}
				
				SetAFactoryPresetAsCurrent(kPresets[i]);
				return noErr;
			}
		}
	}
	
	return kAudioUnitErr_InvalidProperty;
}


#pragma mark ____TremoloUnitEffectKernel

TremoloUnit::TremoloUnitKernel::TremoloUnitKernel(AUEffectBase *inAudioUnit )
:
	AUKernelBase(inAudioUnit),
	mSamplesProcessed(0),
	mCurrentScale(0)
{
	for(int i = 0; i < kWaveArraySize; ++i)
	{
		double radians = i * 2.0 * pi / kWaveArraySize;
		mSine[i] = (sin(radians) + 1.0) * 0.5;
	}
	
	for(int i = 0; i < kWaveArraySize; ++i)
	{
		double radians = i * 2.0 * pi / kWaveArraySize;
		mSquare[i] = (sin(radians) +
			0.3 * sin(3 * radians) +
			0.15 * sin(5 * radians) +
			0.075 * sin(7 * radians) +
			0.0375 * sin(9 * radians) +
			0.01875 * sin(11 * radians) +
			0.009375 * sin(13 * radians) +
			0.8) * 0.63;
	}
	
	mSampleFrequency = GetSampleRate();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//	TremoloUnit::TremoloUnitKernel::Reset()
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void TremoloUnit::TremoloUnitKernel::Reset()
{
	mCurrentScale = 0;
	mSamplesProcessed = 0;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//	TremoloUnit::TremoloUnitKernel::Process
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void TremoloUnit::TremoloUnitKernel::Process(const Float32 	*inSourceP,
											 Float32 *inDestP,
											 UInt32 inFramesToProcess,
											 UInt32 inNumChannels, // for version 2 AudioUnits inNumChannels is always 1
											 bool &ioSilence)
{
	
	if(!ioSilence)
	{
		const Float32 *sourceP = inSourceP;
		
		Float32 *destP = inDestP;
		Float32 inputSample, outputSample, tremoloFrequency, tremoloDepth, samplesPerTremoloCycle, rawTremoloGain, tremoloGain;
		int tremoloWaveform;
		
		tremoloFrequency = GetParameter(kParameter_Frequency);
		tremoloDepth = GetParameter(kParameter_Depth);
		tremoloWaveform = (int)GetParameter(kParameter_Waveform);
		
		if(tremoloWaveform == kSineWave_Tremolo_Waveform)
			waveArrayPointer = &mSine[0];
		else
			waveArrayPointer = &mSquare[0];
		
		if(tremoloFrequency < kMinimumValue_Tremolo_Freq)
			tremoloFrequency = kMinimumValue_Tremolo_Freq;
		if(tremoloFrequency > kMaximumValue_Tremolo_Freq)
			tremoloFrequency = kMaximumValue_Tremolo_Freq;
		
		if(tremoloDepth < kMinimumValue_Tremolo_Depth)
			tremoloDepth = kMinimumValue_Tremolo_Depth;
		if(tremoloDepth > kMaximumValue_Tremolo_Depth)
			tremoloDepth = kMaximumValue_Tremolo_Depth;
		
		if(tremoloWaveform != kSineWave_Tremolo_Waveform && tremoloWaveform != kSquareWave_Tremolo_Waveform)
			tremoloWaveform = kSquareWave_Tremolo_Waveform;
		
		samplesPerTremoloCycle = mSampleFrequency / tremoloFrequency;
		mNextScale = kWaveArraySize / samplesPerTremoloCycle;
		
		// the sample processing loop
		for(int i = inFramesToProcess; i > 0; --i)
		{
			int index = static_cast<long>(mSamplesProcessed * mCurrentScale) % kWaveArraySize;
			
			if((mNextScale != mCurrentScale) && (index == 0))
			{
				mCurrentScale = mNextScale;
				mSamplesProcessed = 0;
			}
			
			if((mSamplesProcessed >= sampleLimit) && (index == 0))
				mSamplesProcessed = 0;
			
			rawTremoloGain = waveArrayPointer[index];
			tremoloGain = (rawTremoloGain * tremoloDepth - tremoloDepth + 100.0) * 0.01;
			inputSample = *sourceP;
			outputSample = (inputSample * tremoloGain);
			*destP = outputSample;
			sourceP += 1;
			destP += 1;
			mSamplesProcessed += 1;
		}
	}
}

