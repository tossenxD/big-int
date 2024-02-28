#ifndef KERNELS
#define KERNELS

#include "ker-helpers.cu.h"

/****************************/
/*** Big-Integer Addition ***/
/****************************/

//* Performs parallel addition of big integers;
//* `m` is size of the big integers in Base::uint_t units; `q` is the sequentialization factor;
//* `ipb` is number of instances of big integers per block.
template<class Base, uint32_t m, uint32_t q, uint32_t ipb>
__global__ void baddKer(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t = typename Base::uint_t;
    __shared__ uint_t sh_mem[m*ipb];
    uint_t ass[q];
    uint_t bss[q];
    uint_t rss[q];
    uint_t css[q];

    // copy from global memory to registers
    cpGlb2Reg<uint_t,m,q,ipb>(sh_mem, as, ass); // TODO add ipb to all copy functions
    __syncthreads();
    cpGlb2Reg<uint_t,m,q,ipb>(sh_mem, bs, bss);
    __syncthreads();
    
    { // this code is morally correct but does not validate
    
        // compute result and carry for each unit
        uint_t acc = SegCarryProp<Base>::identity();
        for(int i=0; i<q; i++) {
            rss[i] = ass[i] + bss[i];
            css[i] = ((uint_t) (rss[i] < ass[i])) | (((uint_t) (rss[i] == Base::HIGHEST)) << 1);
            acc = SegCarryProp<Base>::apply(acc, css[i]);
        }
        
        uint_t last_carry = (threadIdx.x % (m/q) == 0) ? (acc | 4) : acc;
        sh_mem[threadIdx.x] = last_carry;
        __syncthreads();
        scanIncBlock< SegCarryProp<Base> >(sh_mem, threadIdx.x);
        uint_t carry_prefix = (threadIdx.x % (m/q) == 0) ? SegCarryProp<Base>::identity() : sh_mem[threadIdx.x-1];
        __syncthreads();
        
        for(int i=0; i<q; i++) {
            rss[i] += (carry_prefix & 1);
            carry_prefix = SegCarryProp<Base>::apply(carry_prefix, css[i]);
        }

    }
#if 0
    // scan carries
    if (threadIdx.x % (m/q) == 0) css[q-1] += 4; // use third bit as flag
    cpReg2Shm<uint_t,q>(css, sh_mem);
    __syncthreads();
    scanExcBlockBlelloch<SegCarryProp<Base>,m,q>(sh_mem);
    cpShm2Reg<uint_t,q>(sh_mem, css);

    // update result from the propagated carries
    for(int i=0; i<q; i++) rss[i] += (css[i] & 1);
#endif
    cpReg2Glb<uint_t,m,q,ipb>(sh_mem, rss, rs);
}

#endif // BIG_INT_KERNELS
