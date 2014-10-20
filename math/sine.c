#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

double 
factorial(unsigned n) {
    double r = 1;
    for(; n > 0; --n) r *= n;
    return r;
}

double
exponent(double x, int power) {
    double r = 1;
    bool neg = false;
    if (power < 0) {
        neg = true;
        power = -power;
    }
    for(; power > 0; --power) r *= x;
    if (neg) r = 1.0 / r;
    return r;
}

double
sine_internal(double radians, int iterations) {
    int power_fac = 3;
    double sign = -1;
    double r = radians;
    for(; iterations > 0; --iterations) {
        double fac = factorial(power_fac);
        r += sign * exponent(radians, power_fac) / fac;
        power_fac += 2;
        sign = -sign;
    }
    return r;
}

double
sine(double radians) {
    return sine_internal(radians, 15);
}

double const PI = 3.14159265358979323846264338327950288419716939937510;

int
main(int const argc, char const* const* argv) {
    printf("exp(5,3) = %f\n", exponent(5,3));
    printf("exp(5.25,3) = %f\n", exponent(5.25,3));
    printf("exp(100,0) = %f\n", exponent(100,0));
    printf("exp(100,-1) = %f\n", exponent(100,-1));
    printf("exp(10,-3) = %f\n", exponent(10,-3));
    printf("fac(5) = %f\n", factorial(5));
    printf("sin(0) = %f\n", sine(0));
    printf("sin(PI) = %f\n", sine(PI));
    printf("sin(2*PI) = %f\n", sine(2*PI));
    return 0;
}

