#ifndef KERNELS
#define KERNELS

#include "ker-helpers.cu.h"

/****************************/
/*** Big-Integer Addition ***/
/****************************/

template<class Base, int m>
__global__ void baddKer(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    const unsigned int tid = threadIdx.x;
    const unsigned int gid = blockIdx.x * blockDim.x + tid;
    using uint_t = typename Base::uint_t;

    // compute r and c
    uint_t a   = as[gid];
    uint_t b   = bs[gid];
    uint_t r   = a + b;
    uint32_t c = ((uint32_t) (r < a)) | (((uint32_t) (r == Base::HIGHEST)) << 1);

    // scan carries
    __shared__ uint32_t sh_mem[m];
    sh_mem[tid] = c;
    __syncthreads();
    c = scanExcBlock<CarryProp>(sh_mem, tid);
    
    rs[gid] = r + (c & 1);
}

#endif // BIG_INT_KERNELS
