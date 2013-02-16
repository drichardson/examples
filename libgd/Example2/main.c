#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <gd.h>
#include <gdfontg.h>

//#define kFontFilename "/Library/Fonts/Times New Roman.ttf"
//#define kFontFilename "/Library/Fonts/Papyrus.dfont"
//#define kFontFilename "/System/Library/Fonts/Helvetica.dfont"

// Arial Unicode seems to render the most characters. I've tried it with:
// English, French, German, Spanish, Chinese, Korean, Japanese, Hindi, Vietnamese. However, it doesn't render Arabic.
#define kFontFilename "/Library/Fonts/Arial Unicode.ttf"

// Use this font for Arabic.
//#define kFontFilename "/Library/Fonts/Baghdad.ttf"

static void err(const char* msg, ...);

int main (int argc, const char * argv[])
{
	char* stringToPrint = "Douglas Richardson";
	FILE *fpBasePNG = fopen("Test-License.png", "rb");
	if(fpBasePNG == NULL)
		err("Couldn't open png file.");
	
	if(argc >= 2)
		stringToPrint = (char*)argv[1];
	
	gdImagePtr im;
	im = gdImageCreateFromPng(fpBasePNG);
	
	if(im == NULL)
		err("Couldn't read PNG file.");
	
	printf("PNG size is %d width, %d tall\n", im->sx, im->sy);
	
	fclose(fpBasePNG);
	
	int black = gdImageColorAllocate(im, 0, 0, 0);
	gdImageLine(im, 0, 0, 100, 100, black);
	
#if 0
	gdFontPtr font = gdFontGetGiant(); // For nice fonts, use FreeType.
	gdImageString(im, font, 100, 100, (unsigned char*)"Douglas Richardson", black);
#else
	gdFontCacheSetup(); // one time initialization. can pair with gdFontCacheShutdown if appropriate
	
	/*
	 brect is:
	 0	lower left corner, X position
	 1	lower left corner, Y position
	 2	lower right corner, X position
	 3	lower right corner, Y position
	 4	upper right corner, X position
	 5	upper right corner, Y position
	 6	upper left corner, X position
	 7	upper left corner, Y position
	 */
	
	int brect[8];
	
	// Don't think I need to worry about padding since it's just a bunch of ints.
	struct myBrectStruct {
		int lowerLeftX;
		int lowerLeftY;
		int lowerRightX;
		int lowerRightY;
		int upperRightX;
		int upperRightY;
		int upperLeftX;
		int upperLeftY;
	} myBrect;
	
	//gdFTUseFontConfig(gdFTEX_FONTPATHNAME); // Don't call this, it makes fontconfig get used instead of the font path.
	
	char* error = gdImageStringFT(NULL, brect, 0, kFontFilename, 16.0, 0.0, 70, 130, stringToPrint);
	if(error)
		err("Couldn't create image string. %s", error);
	
	printf("Got brect = %d, %d, %d, %d, %d, %d, %d, %d\n",
		   brect[0], brect[1], brect[2], brect[3], brect[4], brect[5], brect[6], brect[7]);
	
	
	// Using my customer structure.
	error = gdImageStringFT(NULL, (int*)&myBrect, 0, kFontFilename, 16.0, 0.0, 70, 130, stringToPrint);
	if(error)
		err("Couldn't create image string. %s", error);
	
	printf("Got brect = %d, %d, %d, %d, %d, %d, %d, %d\n",
		   myBrect.lowerLeftX, myBrect.lowerLeftY, myBrect.lowerRightX, myBrect.lowerRightY,
		   myBrect.upperRightX, myBrect.upperRightY, myBrect.upperLeftX, myBrect.upperLeftY);

	// Calculate the bottom left point to center this image.
	int textWidth = myBrect.lowerRightX - myBrect.lowerLeftX;
	int textHeight = myBrect.lowerRightY - myBrect.upperRightY;	
	int bottomX = (im->sx - textWidth) / 2;
	int bottomY = (im->sy + textHeight) / 2;
	
	printf("Calculated position for text rectangle: (x, y), (w, h) = (%d, %d), (%d, %d)\n", bottomX, bottomY, textWidth, textHeight);
	
	
	//error = gdImageStringFT(im, brect, 0, kFontFilename, 16.0, 0.0, 70, 130, stringToPrint);
	error = gdImageStringFT(im, brect, 0, kFontFilename, 16.0, 0.0, bottomX, bottomY, stringToPrint);
	if(error)
		err("Couldn't create image string. %s", error);
#endif
	
	FILE *pngOut = fopen("out.png", "wb");
	gdImagePng(im, pngOut);
	
	fclose(pngOut);
	
	gdImageDestroy(im);
	
    return 0;
}

static void err(const char* msg, ...)
{
	va_list va;
	va_start(va, msg);
	vfprintf(stderr, msg, va);
	fputc('\n', stderr);
	va_end(va);
	exit(1);
}
