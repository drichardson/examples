#ifndef _INPUT_CONVOLUTIONS_H_
#define _INPUT_CONVOLUTIONS_H_

#include <inttypes.h>

typedef struct
{
	unsigned sample_count;
	int32_t samples[];

} Int32Signal;

Int32Signal *Int32Signal_Alloc(unsigned sample_count);
void Int32Signal_Free(Int32Signal *signal);

// x: the input signal
// h: the impulse response
// return: the output signal (normally called y)
Int32Signal *
Int32Signal_ConvolveUsingInputSideAlgorithm_AllocResult(const Int32Signal *x,
							const Int32Signal *h);

void Int32Signal_Print(const Int32Signal *s);

typedef struct
{
	unsigned sample_count;
	float samples[];

} Float32Signal;

Float32Signal *Float32Signal_Alloc(unsigned sample_count);
void Float32Signal_Free(Float32Signal *signal);

// x: the input signal
// h: the impulse response
// return: the output signal (normally called y)
Float32Signal *Float32Signal_ConvolveUsingInputSideAlgorithm_AllocResult(
    const Float32Signal *x, const Float32Signal *h);
Float32Signal *Float32Signal_ConvolveUsingOutputSideAlgorithm_AllocResult(
    const Float32Signal *x, const Float32Signal *h);

void Float32Signal_Print(const Float32Signal *s);

#endif
