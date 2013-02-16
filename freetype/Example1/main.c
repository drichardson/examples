#include <stdio.h>
#include <ft2build.h>
#include <freetype2/freetype/freetype.h>

#define kFontFilename "/Library/Fonts/Times New Roman.ttf"

static void err(const char* msg);

int main (int argc, const char * argv[])
{
	
	FT_Library library;
	
	FT_Error error = FT_Init_FreeType(&library);
	if(error)
		err("freetype Library initialization error");
	
	
	puts("Going to load font from " kFontFilename);
	FT_Face face;
	error = FT_New_Face(library, kFontFilename, 0, &face);
	
	if(error)
		err("Couldn't load font");
	
	printf("This font has %d faces\n", face->num_faces);
	
    return 0;
}

static void err(const char* msg)
{
	fprintf(stderr, "ERROR: %s\n", msg);
	exit(1);
}
