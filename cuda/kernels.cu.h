#ifndef KERNELS
#define KERNELS

#include "ker-helpers.cu.h"

/****************************/
/*** Big-Integer Addition ***/
/****************************/


template<class Base, int m>
__global__ void baddKer(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t = typename Base::uint_t;
    
    const unsigned int gid = blockIdx.x * blockDim.x + threadIdx.x;
    uint_t a = as[gid];
    uint_t b = bs[gid];
    uint_t r = a + b;
    uint32_t c = ((uint32_t) (r < a)) | (((uint32_t) (r == Base::HIGHEST)) << 1);
    
    rs[gid] = r;
}

#endif // BIG_INT_KERNELS
