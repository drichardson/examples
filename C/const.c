#include <stdio.h>

void nonconst_func(int *p) {}
void const_func1(int const *p) {}
void const_func2(const int *p) {}

int main()
{
	int i = 1;
	i = 2;

	int const j = 1;
	// error: assignment of read-only variable 'j'
	// j = 2;

	int *pi = &i;
	*pi = 3;

	{
	    // warning: initialization discards 'const' qualifier from
	    // pointer target type int* pj = &j; *pj = 4;
	}

	{
		int const *pcj = &j;
		// error: assignment of read-only location '*pc1'
		// *pc1 = 6;
		pcj++;
	}

	{
	    // warning: initialization discards 'const' qualifier of pointer
	    // target type
	    // int* const pcj2 = &j;
	    //*pcj2 = 7;
	    // pcj2++;
	}

	{
		int *const pci = &i;
		*pci = 0;
		// error: increment of read-only variable 'pci'
		// ++pci;
	}

	{
		const int *pci2 = &i;
		// error: assignment of read-only location '*pci2'
		// *pci2 = 0;
		++pci2;
	}

	{
		nonconst_func(&i);
		const_func1(&i);
		const_func2(&i);

		// error: passing argument 1 of ‘nonconst_func’ discards ‘const’
		// qualifier from pointer target type nonconst_func(&j);
		const_func1(&j);
		const_func2(&j);
	}
}
