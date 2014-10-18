#include <stdio.h>
#include <stdbool.h>

int
factorial(int n) {
    int r = 1;
    for(; n > 0; --n) r *= n;
    return r;
}

float
exponent(float x, int power) {
    float r = 1;
    bool neg = false;
    if (power < 0) {
        neg = true;
        power = -power;
    }
    for(; power > 0; --power) r *= x;
    if (neg) r = 1.0 / r;
    return r;
}

float
sine(float radians, int iterations) {
    int power_fac = 3;
    int sign = -1;
    float r = radians;
    for(; iterations > 0; --iterations) {
        float fac = factorial(power_fac);
        r += sign * exponent(radians, power_fac) / fac;
        power_fac += 2;
        sign *= -1;
    }
    return r;
}

double const PI = 3.14159;

int
main(int const argc, char const* const* argv) {
    printf("exp(5,3) = %f\n", exponent(5,3));
    printf("exp(5.25,3) = %f\n", exponent(5.25,3));
    printf("exp(100,0) = %f\n", exponent(100,0));
    printf("exp(100,-1) = %f\n", exponent(100,-1));
    printf("exp(10,-3) = %f\n", exponent(10,-3));
    printf("fac(5) = %d\n", factorial(5));
    printf("sin(0,0) = %f\n", sine(0, 0));
    printf("sin(0,0) = %f\n", sine(PI, 0));
    printf("sin(0,10) = %f\n", sine(PI, 9));
    return 0;
}

