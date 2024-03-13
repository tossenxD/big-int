#ifndef KERNEL_MULTIPLICATION
#define KERNEL_MULTIPLICATION

#include "ker-helpers.cu.h"
#include "ker-add.cu.h"

/****************************************************/
/*** Big Integer Multiplication By Convolution v1 ***/
/****************************************************/

/** Each block multiplies one instance of big integers and each thread handle two units **/

template<class Base, uint32_t m>
__device__ void
convMult1Run(typename Base::uint_t* shmem_as, typename Base::uint_t* shmem_bs,
             typename Base::uint_t* shmem_ls, typename Base::uint_t* shmem_hs,
             typename Base::uint_t* shmem_cs, typename Base::carry_t* shmem_add,
             typename Base::uint_t* rss) {
    using uint_t  = typename Base::uint_t;
    using ubig_t  = typename Base::ubig_t;
    using carry_t = typename Base::carry_t;

    // 1. compute low, high and carry (high overflow) for k1 and k2 (thread elements)
    uint_t lss[2]; uint_t hss[2]; uint_t css[2];
    lss[0] = 0; hss[0] = 0; css[0] = 0;
    lss[1] = 0; hss[1] = 0; css[1] = 0;

    uint64_t k1 = threadIdx.x;
    uint64_t k2 = m-1 - k1;

    for (int i=0; i<=k1; i++) {
        // compute high and low part of result
        int j  = k1 - i;
        ubig_t r  = ((ubig_t) shmem_as[i]) * ((ubig_t) shmem_bs[j]);
        uint_t lr = (uint_t) r;
        uint_t hr = (uint_t) (r >> Base::bits);
        // update l, h and c
        lss[0] += lr;
        hss[0] += hr + (lss[0] < lr);
        css[0] += (hss[0] < (hr + (lss[0] < lr)));
    }
    for (int i=0; i<=k2; i++) {
        // compute high and low part of result
        int j  = k2 - i;
        ubig_t r  = ((ubig_t) shmem_as[i]) * ((ubig_t) shmem_bs[j]);
        uint_t lr = (uint_t) r;
        uint_t hr = (uint_t) (r >> Base::bits);
        // update l, h, and c
        lss[1] += lr;
        hss[1] += hr + (lss[1] < lr);
        css[1] += (hss[1] < (hr + (lss[1] < lr)));
    }
    __syncthreads();

    // 2. write low part, high part and carry part to shared memory 
    shmem_ls[k1] = lss[0]; shmem_hs[k1] = hss[0]; shmem_cs[k1] = css[0];
    shmem_ls[k2] = lss[1]; shmem_hs[k2] = hss[1]; shmem_cs[k2] = css[1];
    __syncthreads();

    // 3. fetch low, high and carry from shared memory to registers
    lss[0] = shmem_ls[threadIdx.x*2];
    lss[1] = shmem_ls[threadIdx.x*2 + 1];
    hss[0] = (threadIdx.x) ? shmem_hs[threadIdx.x*2 - 1] : 0;
    hss[1] = shmem_hs[threadIdx.x*2];
    css[0] = (threadIdx.x) ? shmem_cs[threadIdx.x*2 - 2] : 0;
    css[1] = (threadIdx.x) ? shmem_cs[threadIdx.x*2 - 1] : 0;
    __syncthreads();

    // 4. add low, high and carry
    baddKer2Run<Base,m,2>(lss, hss, rss, shmem_add);
    __syncthreads();
    baddKer2Run<Base,m,2>(css, rss, rss, shmem_add);
}

template<class Base, uint32_t m>
__global__ void
convMult1(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy as and bs into shared memory
    __shared__ uint_t shmem[m*3];
    cpGlb2Shm<uint_t,m,2,1>(as, shmem);
    cpGlb2Shm<uint_t,m,2,1>(bs, shmem+m);
    __syncthreads();

    // 2. multiply as and bs
    uint_t rss[2];
    convMult1Run<Base,m>(shmem, shmem+m, shmem, shmem+m, shmem+2*m, (carry_t*)shmem, rss);
    __syncthreads();

    // 3. write result to global memory
    cpReg2Glb<uint_t,m,2,1>(rss, rs);
}

template<class Base, uint32_t m, uint32_t p> // runs p multiplications
__global__ void
convMult1Bench(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy as and bs into shared memory
    __shared__ uint_t shmem[m*5];
    cpGlb2Shm<uint_t,m,2,1>(as, shmem);
    cpGlb2Shm<uint_t,m,2,1>(bs, shmem+m);
    __syncthreads();

    // 2. multiply as and bs p times
    uint_t rss[2];
    for(int i=0; i<p; i++) {
        convMult1Run<Base,m>(shmem,shmem+m,shmem+2*m,shmem+3*m,shmem+4*m,(carry_t*)shmem+2*m,rss);
        __syncthreads();
    }

    // 3. write result to global memory
    cpReg2Glb<uint_t,m,2,1>(rss, rs);
}

/****************************************************/
/*** Big Integer Multiplication By Convolution v2 ***/
/****************************************************/

/** Each block multiplies one instance of big integers and each thread handle q units **/

template<class Base, uint32_t m, uint32_t q>
__device__ void
convMult2Run(typename Base::uint_t* shmem_as, typename Base::uint_t* shmem_bs,
             typename Base::uint_t* shmem_ls, typename Base::uint_t* shmem_hs,
             typename Base::uint_t* shmem_cs, typename Base::carry_t* shmem_add,
             typename Base::uint_t* rss) {
    using uint_t  = typename Base::uint_t;
    using ubig_t  = typename Base::ubig_t;
    using carry_t = typename Base::carry_t;

    // 1. compute low, high and carry (high overflow) for k1 and k2 (thread elements)
    uint_t lss[q]; uint_t hss[q]; uint_t css[q];
    for (int i=0; i<q; i++) {
        lss[i] = 0; hss[i] = 0; css[i] = 0;
    }

    for (int s=0; s<q/2; s++) {
        uint64_t k1 = threadIdx.x*(q/2) + s;
        uint64_t k2 = m-1 - k1;
    
        for (int i=0; i<=k1; i++) {
            // compute high and low part of result
            int j  = k1 - i;
            ubig_t r  = ((ubig_t) shmem_as[i]) * ((ubig_t) shmem_bs[j]);
            uint_t lr = (uint_t) r;
            uint_t hr = (uint_t) (r >> Base::bits);
            // update l, h and c
            lss[s*2] += lr;
            hss[s*2] += hr + (lss[s*2] < lr);
            css[s*2] += (hss[s*2] < (hr + (lss[s*2] < lr)));
        }
        for (int i=0; i<=k2; i++) {
            // compute high and low part of result
            int j  = k2 - i;
            ubig_t r  = ((ubig_t) shmem_as[i]) * ((ubig_t) shmem_bs[j]);
            uint_t lr = (uint_t) r;
            uint_t hr = (uint_t) (r >> Base::bits);
            // update l, h, and c
            lss[s*2+1] += lr;
            hss[s*2+1] += hr + (lss[s*2+1] < lr);
            css[s*2+1] += (hss[s*2+1] < (hr + (lss[s*2+1] < lr)));
        }
    }
    __syncthreads();

    // 2. write low part, high part and carry part to shared memory
    for (int s=0; s<q/2; s++) {
        uint64_t k1 = threadIdx.x*(q/2) + s;
        uint64_t k2 = m-1 - k1;

        shmem_ls[k1] = lss[s*2]; shmem_hs[k1] = hss[s*2]; shmem_cs[k1] = css[s*2];
        shmem_ls[k2] = lss[s*2+1]; shmem_hs[k2] = hss[s*2+1]; shmem_cs[k2] = css[s*2+1];
    }
    __syncthreads();

    // 3. fetch low, high and carry from shared memory to registers
    for (int i=0; i<q; i++) {
        uint64_t off = threadIdx.x*q + i;
        lss[i] = shmem_ls[off];
        hss[i] = (off)     ? shmem_hs[off-1] : 0;
        css[i] = (off > 1) ? shmem_cs[off-2] : 0;
    }
    __syncthreads();

    // 4. add low, high and carry
    baddKer2Run<Base,m,q>(lss, hss, rss, shmem_add);
    __syncthreads();
    baddKer2Run<Base,m,q>(css, rss, rss, shmem_add);
}

template<class Base, uint32_t m, uint32_t q>
__global__ void
convMult2(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy as and bs into shared memory
    __shared__ uint_t shmem[m*3];
    cpGlb2Shm<uint_t,m,q,1>(as, shmem);
    cpGlb2Shm<uint_t,m,q,1>(bs, shmem+m);
    __syncthreads();

    // 2. multiply as and bs
    uint_t rss[q];
    convMult2Run<Base,m,q>(shmem, shmem+m, shmem, shmem+m, shmem+2*m, (carry_t*)shmem, rss);
    __syncthreads();

    // 3. write result to global memory
    cpReg2Glb<uint_t,m,q,1>(rss, rs);
}

template<class Base, uint32_t m, uint32_t q, uint32_t p> // runs p multiplications
__global__ void
convMult2Bench(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy as and bs into shared memory
    __shared__ uint_t shmem[m*5];
    cpGlb2Shm<uint_t,m,q,1>(as, shmem);
    cpGlb2Shm<uint_t,m,q,1>(bs, shmem+m);
    __syncthreads();

    // 2. multiply as and bs p times
    uint_t rss[q];
    for(int i=0; i<p; i++) {
        convMult2Run<Base,m,q>(shmem,shmem+m,shmem+2*m,shmem+3*m,shmem+4*m,(carry_t*)shmem+2*m,rss);
        __syncthreads();
    }

    // 3. write result to global memory
    cpReg2Glb<uint_t,m,q,1>(rss, rs);
}

#endif // KERNEL_MULTIPLICATION
