#include <string.h>

int bar(int x) {
	return x * 2;
}

int baz(int x, long y) {
	return x + y * 2;
}

int fuzz(char const* s, long y) {
	return baz(bar(strlen(s)), y);
}
