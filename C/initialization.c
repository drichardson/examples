#include <stdio.h>

struct mystruct
{
	int x, y;
	int z;
};

void print_mystruct(struct mystruct const *s)
{
	printf("{x=%d, y=%d, z=%d}\n", s->x, s->y, s->z);
}

int main(int argc, char *const *argv)
{

	// Since partial initialization zero initializes the rest of the
	// members, {0} can be used to zero an entire structure.
	// For more information, see:
	//   https://en.cppreference.com/w/c/language/initialization
	//   https://en.cppreference.com/w/c/language/struct_initialization
	struct mystruct zero = {0};
	print_mystruct(&zero);

	struct mystruct s1 = {1, 2, 3};
	struct mystruct s2 = {.z = 30, .y = 20, .x = 10};
	struct mystruct s3 = {.y = 200, 300, .x = 100};

	print_mystruct(&s1);
	print_mystruct(&s2);
	print_mystruct(&s3);

	return 0;
}
