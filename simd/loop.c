void variable_iteration_loop(float* a, float* b, float fac, float c, int n) {
    for(int i = 0; i < n; ++i) {
        a[i] = fac * b[i] + c;
    }
}

void variable_iteration_loop_restrict(float* restrict a, float* restrict b, float fac, float c, int n) {
    for(int i = 0; i < n; ++i) {
        a[i] = fac * b[i] + c;
    }
}

void fixed100_iteration_loop(float* a, float* b, float fac, float c) {
    for(int i = 0; i < 100; ++i) {
        a[i] = fac * b[i] + c;
    }
}

void fixed100_iteration_loop_restrict(float* restrict a, float* restrict b, float fac, float c) {
    for(int i = 0; i < 100; ++i) {
        a[i] = fac * b[i] + c;
    }
}
