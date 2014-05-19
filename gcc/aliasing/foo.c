int foo(int x)
{
    int a, b;
    a = 26;
    b = 31;
    int *p;
    p = x > 10 ? &a : &b;
    *p += x;
    return *p;
}

