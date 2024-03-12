#ifndef KERNEL_MULTIPLICATION
#define KERNEL_MULTIPLICATION

#include "ker-helpers.cu.h"
#include "ker-add.cu.h"

template<class Base, uint32_t m>
__global__ void
convMult(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t = typename Base::uint_t;
    using ubig_t = typename Base::ubig_t;
    using carry_t = typename Base::carry_t;
    __shared__ uint_t shmem[m*3];

    // 1. copy as and bs into shared memory
    uint_t* shmem_as = shmem;
    uint_t* shmem_bs = shmem + m;
    cpGlb2Shm<uint_t,m,1,1>(as, shmem_as);
    __syncthreads();
    cpGlb2Shm<uint_t,m,1,1>(bs, shmem_bs);

    // 2. compute low, high and carry (high overflow) for k1 and k2 (thread elements)
    uint64_t k1 = threadIdx.x + 1;
    uint64_t k2 = m - k1;
    ubig_t HIGHEST = (ubig_t) Base::HIGHEST;
    
    ubig_t h1; ubig_t l1;
    ubig_t h2; ubig_t l2;

    for (int i=0; i<k1; i++) {
        int j = k1 - i;
        ubig_t r = shmem_as[i] * shmem_bs[j];
        l1 += r & HIGHEST;
        h1 += r >> Base::bits;
    }
    for (int i=0; i<k2; i++) {
        int j = k2 - i;
        ubig_t r = shmem_as[i] * shmem_bs[j];
        l2 += r & HIGHEST;
        h2 += r >> Base::bits;
    }

    h1 += l1 / HIGHEST;       // low overflows to high
    h2 += l2 / HIGHEST;
    uint_t c1 = h1 / HIGHEST; // high overflows to carry
    uint_t c2 = h2 / HIGHEST;

    // 3. write low part, high part and carry part to shared memory 
    uint_t* shmem_ls = shmem;
    uint_t* shmem_hs = shmem + m;
    uint_t* shmem_cs = shmem + 2*m;

    shmem_ls[k1-1] = (uint_t) l1;
    shmem_ls[k2-1] = (uint_t) l2;
    shmem_hs[k1-1] = (uint_t) h1;
    shmem_hs[k2-1] = (uint_t) h2;
    shmem_cs[k1-1] = c1;
    shmem_cs[k2-1] = c2;

    // 4. add low and high, then, add result and carry
    const uint32_t q = 2;
    uint_t hss[q];
    uint_t lss[q];
    uint_t css[q];
    for (int i=0; i<q; i++) {
        uint64_t off = threadIdx.x*q + i;
        hss[i] = off ? shmem_hs[off - 1] : 0;
        lss[i] = shmem_ls[off];
        css[i] = (off > 1) ? shmem_cs[off - 2] : 0;
    }
    uint_t rss[q];
    carry_t* shmem_carry = (carry_t*) shmem;
    baddKer3Run<Base,m,q,1>(lss, hss, rss, shmem_carry);
    baddKer3Run<Base,m,q,1>(css, rss, rss, shmem_carry);

    cpReg2Glb<uint_t,m,q,1>(rss, rs);
}

#endif // KERNEL_MULTIPLICATION
