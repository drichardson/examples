#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// https://en.wikipedia.org/wiki/Flexible_array_member
// https://en.cppreference.com/w/c/language/array

struct mystruct_t
{
	int n;
	double d[]; // flexible array member. Must be last element.
};

struct mystruct_t *mystruct_alloc(int n)
{
	struct mystruct_t *s =
	    malloc(sizeof(struct mystruct_t) + (sizeof(double) * n));

	s->n = n;
	for (int i = 0; i < n; ++i)
	{
		s->d[i] = (double)i / (double)n;
	}

	return s;
}

void mystruct_print(struct mystruct_t const *s)
{
	for (int i = 0; i < s->n; ++i)
	{
		printf("d[%d]=%f\n", i, s->d[i]);
	}
}

int main()
{
	struct mystruct_t *s1 = mystruct_alloc(8);
	struct mystruct_t *s2 = mystruct_alloc(100);

	puts("s1 ===========");
	mystruct_print(s1);

	puts("s2 ===========");
	mystruct_print(s2);

	free(s1);
	free(s2);

	return 0;
}
