int bar(int x);
int baz(int x, long y);
int fuzz(char const* s, long y);

int main(int argc, char const** argv) {
	fuzz(argv[1], 1234);
	return 0;
}
