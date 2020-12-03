#include <stdio.h>
#include <setjmp.h>

static void f1(void);
static void f2(void);
static void f3(void);
static void f4(void);
static void f5(void);

static jmp_buf jb;

int main (int argc, const char * argv[])
{
	puts("main 1");
	
	int rc = setjmp(jb);
	
	if(rc == 0)
	{
		puts("Set jump buffer");
		f1();
		puts("Returned from f1");
	}
	else
		printf("Returned from jump with %d", rc);
	
    return 0;
}

static void f1(void)
{
	printf("%s begin\n", __func__);
	f2();
	printf("%s end\n", __func__);
}

static void f2(void)
{
	printf("%s begin\n", __func__);
	f3();
	printf("%s end\n", __func__);
}

static void f3(void)
{
	printf("%s begin\n", __func__);
	f4();
	printf("%s end\n", __func__);
}

static void f4(void)
{
	printf("%s begin\n", __func__);
	f5();
	printf("%s end\n", __func__);
}

static void f5(void)
{
	printf("%s begin\n", __func__);
	longjmp(jb, 20);
	printf("%s end\n", __func__);
}
