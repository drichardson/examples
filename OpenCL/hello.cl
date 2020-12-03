__kernel void
square(__global float *in,
      __global float *out,
      int count)
{
    int id = get_global_id(0);
    for(int i = 0; i < count; ++i) {
        out[id+i] = pow(in[id+i], 2);
    }
}
