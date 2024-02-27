#ifndef KERNELS
#define KERNELS

#include "ker-helpers.cu.h"

/****************************/
/*** Big-Integer Addition ***/
/****************************/

template<class Base, uint32_t m, uint32_t q> // m is the size in Base::uint_t units and q the seq-factor
__global__ void baddKer(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t = typename Base::uint_t;
    __shared__ uint_t sh_mem[m];
    uint_t ass[q];
    uint_t bss[q];
    uint_t rss[q];
    uint_t css[q];

    // copy from global memory to registers
    cpGlb2Reg<uint_t,m,q>(sh_mem, as, ass);
    cpGlb2Reg<uint_t,m,q>(sh_mem, bs, bss);
    __syncthreads();

    // compute result and carry for each unit
    for(int i=0; i<q; i++) {
        rss[i] = ass[i] + bss[i];
        css[i] = ((uint_t) (rss[i] < ass[i])) | (((uint_t) (rss[i] == Base::HIGHEST)) << 1);
    }
    __syncthreads();

    // scan carries
    cpReg2Shm<uint_t,q>(css, sh_mem);
    __syncthreads();
    scanExcBlockBlelloch<CarryProp<Base>,m,q>(sh_mem);
    cpShm2Reg<uint_t,q>(sh_mem, css);

    // update result from the propagated carries
    for(int i=0; i<q; i++) rss[i] += (css[i] & 1);
    cpReg2Glb<uint_t,m,q>(sh_mem, rss, rs);
}

#endif // BIG_INT_KERNELS
