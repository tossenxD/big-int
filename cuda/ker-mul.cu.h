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
    cpGlb2Shm<uint_t,m,1,1>(bs, shmem_bs);
    __syncthreads();

    // 2. compute low, high and carry (high overflow) for k1 and k2 (thread elements)
    uint64_t k1 = threadIdx.x;
    uint64_t k2 = m-1 - k1;

    uint_t l1 = 0; uint_t h1 = 0; uint_t c1 = 0;
    uint_t l2 = 0; uint_t h2 = 0; uint_t c2 = 0;

    for (int i=0; i<=k1; i++) {
        int j = k1 - i;
        ubig_t r  = shmem_as[i] * shmem_bs[j];
        uint_t lr = (uint_t) r;
        uint_t hr = (uint_t) (r >> Base::bits);
        l1 += lr;
        h1 += hr + (l1 < lr);
        c1 += (h1 < hr);
    }
    for (int i=0; i<=k2; i++) {
        int j = k2 - i;
        ubig_t r  = shmem_as[i] * shmem_bs[j];
        uint_t lr = (uint_t) r;
        uint_t hr = (uint_t) (r >> Base::bits);
        l2 += lr;
        h2 += hr + (l2 < lr);
        c2 += (h2 < hr);
    }
    __syncthreads();

    // 3. write low part, high part and carry part to shared memory 
    uint_t* shmem_ls = shmem;
    uint_t* shmem_hs = shmem + m;
    uint_t* shmem_cs = shmem + 2*m;

    shmem_ls[k1] = l1; shmem_hs[k1] = h1; shmem_cs[k1] = c1;
    shmem_ls[k2] = l2; shmem_hs[k2] = h2; shmem_cs[k2] = c2;
    __syncthreads();

    // 4. add low and high, then, add result and carry
    const uint32_t q = 2;
    uint_t hss[q];
    uint_t lss[q];
    uint_t css[q];
    for (int i=0; i<q; i++) {
        uint64_t off = threadIdx.x*q + i;
        lss[i] = shmem_ls[off];
        hss[i] = off ? shmem_hs[off-1] : 0;
        css[i] = (off > 1) ? shmem_cs[off-2] : 0;
    }
    __syncthreads();
    uint_t rss[q];
    carry_t* shmem_carry = (carry_t*) shmem;
    baddKer3Run<Base,m,q,1>(lss, hss, rss, shmem_carry);
    __syncthreads();
    baddKer3Run<Base,m,q,1>(css, rss, rss, shmem_carry);
    __syncthreads();

    // 5. write result to global memory
    cpReg2Glb<uint_t,m,q,1>(rss, rs);
}

#endif // KERNEL_MULTIPLICATION
