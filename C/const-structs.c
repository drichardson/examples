/**
  * ===========================================================================
  * Summary
  * ===========================================================================
  * Investigate const pointer to struct.
  *
  * ===========================================================================
  * Result
  * ===========================================================================
  * 
  */

#include <stdio.h>

struct s1 {
    int i1;
};

int main(int argc, char** argv) {
    struct s1 s;
    s.i1 = 5;

    struct s1* p = &s;
    struct s1 const* pc = &s;
    struct s1 * const pc2 = &s;

    p->i1++;
    p = &s;
    // pc->i1++; // illegal, read only variable
    pc2->i1++;
    // pc2 = &s; // illegal, read only variable 

    (void)s;
    (void)pc;
    (void)pc2;

    return 0;
}
