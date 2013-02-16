/*
 *  table-6-1-convolution.c
 *  BookExamples
 *
 *  Created by Doug on 3/27/08.
 *  Copyright 2008 Douglas Richardson. All rights reserved.
 *
 */

#include "6-convolution.h"
#include <strings.h>
#include <stdlib.h>
#include <stdio.h>

Int32Signal* Int32Signal_Alloc(unsigned sample_count)
{
	size_t size = sizeof(Int32Signal) + sizeof(int32_t) * (sample_count - 1);
	Int32Signal *result = (Int32Signal*)malloc(size);
	bzero(result->samples, size);
	result->sample_count = sample_count;
	return result;
}

void Int32Signal_Free(Int32Signal *signal)
{
	free(signal);
}

Int32Signal*
Int32Signal_ConvolveUsingInputSideAlgorithm_AllocResult(const Int32Signal *xSignal,
														const Int32Signal *hSignal)
{
	Int32Signal *ySignal = Int32Signal_Alloc(xSignal->sample_count + hSignal->sample_count - 1);
	int32_t *y = ySignal->samples;
	int32_t *x = xSignal->samples;
	int32_t *h = hSignal->samples;
	
	int i, j;
	
	/*
	 * The impulse response, h, is a description of what an impulse sample in x will do to the system.
	 * The impulse sample in x is only for 1 instant, but it's effects last for the number of samples
	 * in the impulse response. The impulse response defines the path and the input defines the magnitude of
	 * that path. The impulse response must then be computed from the next sample and the result added
	 * to the accumulated signal in y. It is added back into why because the first impulse still has
	 * an effect on the result, since the impulse response is longer than 2 samples.
	 */
	for(i = 0; i < xSignal->sample_count; ++i) {
		for(j = 0; j < hSignal->sample_count; ++j) {
			y[i + j] = y[i + j] + x[i] * h[j];
		}
	}
	
	return ySignal;
}

void Int32Signal_Print(const Int32Signal* s)
{
	int i;
	for(i = 0; i < s->sample_count; ++i)
	{
		if(i > 0)
			putchar(' ');
		printf("%d", s->samples[i]);
	}
}


Float32Signal* Float32Signal_Alloc(unsigned sample_count)
{
	size_t size = sizeof(Float32Signal) + sizeof(int32_t) * (sample_count - 1);
	Float32Signal *result = (Float32Signal*)malloc(size);
	bzero(result->samples, size);
	result->sample_count = sample_count;
	return result;
}

void Float32Signal_Free(Float32Signal *signal)
{
	free(signal);
}

Float32Signal*
Float32Signal_ConvolveUsingInputSideAlgorithm_AllocResult(const Float32Signal *xSignal,
														  const Float32Signal *hSignal)
{
	Float32Signal *ySignal = Float32Signal_Alloc(xSignal->sample_count + hSignal->sample_count - 1);
	float *y = ySignal->samples;
	float *x = xSignal->samples;
	float *h = hSignal->samples;
	
	int i, j;
	
	for(i = 0; i < xSignal->sample_count; ++i) {
		for(j = 0; j < hSignal->sample_count; ++j) {
			y[i + j] = y[i + j] + x[i] * h[j];
		}
	}
	
	return ySignal;
}

Float32Signal*
Float32Signal_ConvolveUsingOutputSideAlgorithm_AllocResult(const Float32Signal *xSignal,
														   const Float32Signal *hSignal)
{
	Float32Signal *ySignal = Float32Signal_Alloc(xSignal->sample_count + hSignal->sample_count - 1);
	float *y = ySignal->samples;
	float *x = xSignal->samples;
	float *h = hSignal->samples;
	
	int i, j;
	
	for(i = 0; i < ySignal->sample_count; ++i) {
		for(j = 0; j < hSignal->sample_count; ++j) {
			if(i - j >= 0 && i - j < xSignal->sample_count)
				y[i] = y[i] +  h[j] * x[i - j];
		}
	}
	
	return ySignal;
}

void Float32Signal_Print(const Float32Signal* s)
{
	int i;
	for(i = 0; i < s->sample_count; ++i)
	{
		if(i > 0)
			putchar(' ');
		printf("%.2f", s->samples[i]);
	}
}