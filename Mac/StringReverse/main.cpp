#include <iostream>

using namespace std;

static void myReverse(char *str, size_t len)
{
	printf("%d\n", len);
	len--;
    /*
	 now this for is a bit unconventional at first glance because there
	 are 2 variables that we're changing values of: i++ and len--.
	 the order of them is irrelevant. so basicaly we're going with i from 
	 start to end of the array. with len we're shortening the array by one
	 each time. this is probably understandable.
	 */
    for (int i = 0; i < len; i++, len--)
    {
        /*
		 now this is the tricky part people that should know about it don't.
		 look at the table below to see what's going on exactly here.
		 */
        str[i] ^= str[len];
        str[len] ^= str[i];
        str[i] ^= str[len];
    }
}

static char* Reverse2(const char* x, size_t xlen)
{
    char *charArray = new char[xlen];
    int len = xlen - 1;
    for (int i = 0; i <= len; i++)
        charArray[i] = x[len-i];
    return charArray;
}

int main (int argc, char * const argv[]) {
    char s1[] = "This is a test.";
	char s2[] = "This is a test2.";
	size_t s1Len = sizeof(s1) - 1;
	size_t s2Len = sizeof(s2) - 1;
	
	cout << "s1: '" << s1 << "'" << endl;
	cout << "s2: '" << s2 << "'" << endl;
	
	char *s1R = Reverse2(s1, s1Len);
	char *s2R = Reverse2(s2, s2Len);
	
	cout << "s1 R2: '" << s1R << "'" << endl;
	cout << "s2 R2: '" << s2R << "'" << endl;
	
	myReverse(s1, s1Len);
	myReverse(s2, s2Len);
	
	cout << "s1: '" << s1 << "'" << endl;
	cout << "s2: '" << s2 << "'" << endl;
	
    return 0;
}
