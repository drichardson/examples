#include <stdio.h>
#include "6-convolution.h"
#include <math.h>

static void int32_convolution()
{
	Int32Signal *x = Int32Signal_Alloc(10);
	Int32Signal *h = Int32Signal_Alloc(5);
	
	int i;
	for(i = 0; i < x->sample_count; ++i)
		x->samples[i] = i % 4;
	for(i = 0; i < h->sample_count; ++i)
		h->samples[i] = i * 2;
	
	Int32Signal *y = Int32Signal_ConvolveUsingInputSideAlgorithm_AllocResult(x, h);
	
	printf("x: ");
	Int32Signal_Print(x);
	
	printf("\nh: ");
	Int32Signal_Print(h);
	
	printf("\ny: ");
	Int32Signal_Print(y);
	
	Int32Signal_Free(x);
	Int32Signal_Free(h);
	Int32Signal_Free(y);
};


static void float32_convolution()
{
	Float32Signal *x = Float32Signal_Alloc(80);
	Float32Signal *h = Float32Signal_Alloc(30);
	
	int i;
	for(i = 0; i < x->sample_count; ++i)
		x->samples[i] = sinf(2 * M_PI * i / 20.0) * 4 * i / ((float)x->sample_count);

	h->samples[h->sample_count / 2] = 1.0; // A high-pass filter.
	
	//Float32Signal *y = Float32Signal_ConvolveUsingInputSideAlgorithm_AllocResult(x, h);
	Float32Signal *y = Float32Signal_ConvolveUsingOutputSideAlgorithm_AllocResult(x, h);
	
	printf("x: ");
	Float32Signal_Print(x);
	
	printf("\nh: ");
	Float32Signal_Print(h);
	
	printf("\ny: ");
	Float32Signal_Print(y);
	
	Float32Signal_Free(x);
	Float32Signal_Free(h);	
	Float32Signal_Free(y);
};


int main (int argc, const char * argv[]) {
	
	printf("INT32 Convolution\n");
	int32_convolution();
	
	printf("\n\nFLOAT Convolution\n");
	float32_convolution();
	
    return 0;
}
