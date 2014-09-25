/**
  * ===========================================================================
  * Abstract 
  * ===========================================================================
  * Perform some compile time fixed size array experiments to see what warnings
  * and errors compilers provide. Use gcc 4.9.1 and clang 3.4.2.
  *
  * ===========================================================================
  * Results: 
  * ===========================================================================
  *
  * ---------------------------------------------------------------------------
  * Experiments with fixed sized arrays using gcc (GCC) 4.9.1
  * ---------------------------------------------------------------------------
  * Calls to functions declared with the form typeX foo(typeY a[N]) are
  * ONLY compile time checked for uninitialized values if the function
  * definition is in the same module as the caller. However, array lengths are
  * NOT checked. So if you provide an N+1 length array to the a[N] function
  * above there will be no warning or error.
  *
  * Calls to functions declared with the form typeX foo(typeY (*p)[N]) will
  * give you an incompatible pointer type error if a pointer to an array of a
  * different type, dimention, or constness is used.
  *
  * Neither fixed sized form prevents you from exceeding the array bounds
  * inside the function definition (unless the caller happens to be in the same
  * module at which point you'll get an uninitialized value error.
  *
  * When using the pointer to array technique, the compiler will not
  * automatically convert a non-const version to a const version.
  * For example, given void foo(char const (*p)[4]), a call to it
  * with foo(a), where char a[4] will fail with incompatible pointer type.
  *
  * When using the point to first element of array technique
  * (typeX foo(typeY a[N])) gcc will automatically convert from non-const
  * to const versions.
  *
  * Fixed size arrays are passed by reference.
  *
  * ---------------------------------------------------------------------------
  * Experiments with clang 3.4.2
  * ---------------------------------------------------------------------------
  * Similar results to gcc, with the exception that clang automatically
  * converts from non-const to const types that are otherwise identical.
  *
  * ===========================================================================
  * Summary
  * ===========================================================================
  * C11 draft section 6.7.3 (Type qualifiers) paragraph that starts with
  * "For two qualified types to be compatible..." suggests that
  * two qualified types (e.g., char * const and char const*) are incomptable
  * unless they are identally qualified. I do not, however, see anything
  * talking about compatibility of non-qualified types with qualified types.
  * Semantically, I'd guess allowing conversion from non-const to otherwise
  * identical const types is safe in all situations, but gcc doesn't always
  * allow this, at least not when the form of the pointer to a fixed size
  * array (e.g., gcc will not make char (*p)[4] compatible with
  * char const (*p)[4]).
  *
  * Some stack overflow post suggest that either gcc got it right or that the
  * standard is open to interpretation.
  * http://stackoverflow.com/questions/17122727/assignment-pointer-to-array-of-constants-pointer-to-array-incompatible-po
  * Specifically this answer: http://stackoverflow.com/a/17123107/196964
  * Also read this: http://c-faq.com/ansi/constmismatch.html
  */
#include <stdio.h>

#if 1
int sum_3(char a[3]);
#else
static int sum_3(char a[3]) {
    return a[0] + a[1] + a[2];
}
#endif

#if 1
int sum_4(char const (*p)[4]);
#else
static int sum_4(char (*p)[4]) {
    return (*p)[0] + (*p)[1] + (*p)[2] + (*p)[3] + (*p)[4] + (*p)[5];
}
#endif

void a3_modify(char a[3]) {
    a[0] += 1;
}

int main(int argc, char const** argv) {

    // ******************************
    // Test form typeX foo(typeY a[N])
    // ******************************

    // compile time error: excess delimiter char a[3] = { 1, 2, 4, 8 };
    char a3[3] = { 1, 2, 4 };
    printf("sum_3(a3) = %d\n", sum_3(a3));

    char a4[4] = { 1, 2, 4, 8 }; 
    // printf("sum_3(a4) = %d\n", sum_3(a4)); // no compile time failure here

#if 0
    // compile time failure when sum_3 is in same module as caller
    // but *NO* compile time failure when sum_3 is in another module
    char a2[2] = { 1, 2 };
    printf("sum_3(a2) = %d\n", sum_3(a2));
#endif

    // **********************************
    // Test form typeX foo(typeY (*p)[N])
    // **********************************

    printf("sum_4(&a4) = %d\n", sum_4((char const (*)[4])&a4));

    // compile time error: incompatible pointer type printf("sum_4(&a3) = %d\n", sum_4(&a3));
    // unsigned char u4[4] = { 1, 2, 4, 8};
    // compile time error: incompatible pointer type printf("sum_4(&u4) = %d\n", sum_4(&u4));
    char const c4[4] = { 1, 2, 4, 8 };
    printf("sum_4(&c4) = %d\n", sum_4(&c4));

    // by value or reference?
    char a3_0_prev = a3[0];
    a3_modify(a3);
    printf("a3[0] = %hhd (was passed by %s)\n", a3[0], a3[0] != a3_0_prev ? "reference" : "value");

#if 1
    // gcc says incompatible pointer type
    // clang allows
    char (*p1)[4] = NULL;
    char const (*p1c)[4] = p1;
    printf("%p\n", p1c);
#endif

    return 0;
}

