// x64 intrinsics
// https://docs.microsoft.com/en-us/cpp/intrinsics/x64-amd64-intrinsics-list

#include <intrin.h>
#include <iostream>

using namespace std;

static void BitScan(unsigned long Mask)
{
	// https://docs.microsoft.com/en-us/cpp/intrinsics/bitscanforward-bitscanforward64
	unsigned long Index = 0;
	if (_BitScanForward(&Index, Mask)) {
		cout << "Found bit at index " << Index << "\n";
	} else {
		cout << "Didn't find bit.\n";
	}
}

void print(const __m128i & a, const char* msg)
{
	cout << msg << ": "
		<< _mm_extract_epi32(a, 0) << ", "
		<< _mm_extract_epi32(a, 1) << ", "
		<< _mm_extract_epi32(a, 2) << ", "
		<< _mm_extract_epi32(a, 3) << " "
		<< "\n";
}

int main()
{
    cout << "intrinsics starting...\n";

	BitScan(0);
	BitScan(1);
	BitScan(0b10000);

	__m128i a = _mm_setzero_si128();
	print(a, "a");

	__m128i b = _mm_setr_epi32(1, 2, 3, 4);
	print(b, "b");

	__m128i c = _mm_setr_epi32(1, 10, 100, 1000);
	print(c, "c");

	__m128i d = _mm_add_epi32(b, c);
	print(d, "b+c");

	__m128i bc = _mm_mul_epi32(b, c);
	print(bc, "b*c");

#if 0	
	for(int i = 0; i < 4; ++i)
	{
		print(_mm_shuffle_epi32(b, i), "X");
	}
#else

	print(_mm_shuffle_epi32(b, 0x00), "X");
	print(_mm_shuffle_epi32(b, 0x11), "X");
	print(_mm_shuffle_epi32(b, 0x22), "X");
	print(_mm_shuffle_epi32(b, 0x33), "X");
	print(_mm_shuffle_epi32(b, 0x44), "X");
	print(_mm_shuffle_epi32(b, 0x55), "X");
	print(_mm_shuffle_epi32(b, 0x66), "X");
	print(_mm_shuffle_epi32(b, 0x77), "X");
	print(_mm_shuffle_epi32(b, 0x88), "X");
	print(_mm_shuffle_epi32(b, 0x99), "X");
	print(_mm_shuffle_epi32(b, 0xaa), "X");
	print(_mm_shuffle_epi32(b, 0xbb), "X");
	print(_mm_shuffle_epi32(b, 0xcc), "X");
	print(_mm_shuffle_epi32(b, 0xdd), "X");
	print(_mm_shuffle_epi32(b, 0xee), "X");
	print(_mm_shuffle_epi32(b, 0xff), "X");


	print(_mm_shuffle_epi32(b, 0b00000000), "X");
	print(_mm_shuffle_epi32(b, 0b11111111), "X");
	print(_mm_shuffle_epi32(b, 0b11100100), "X");
	print(_mm_shuffle_epi32(b, 0b00011011), "X");
#endif

	// _mm_shuffle_epi32 

	// _mm_extract_epi32 
	

    return 0;
}

