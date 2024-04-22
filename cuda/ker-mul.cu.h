#ifndef KERNEL_MULTIPLICATION
#define KERNEL_MULTIPLICATION

#include "ker-helpers.cu.h"
#include "ker-add.cu.h"

/***************/
/*** Helpers ***/
/***************/

/* Converts low-high parts of type `Base::ubig_t` to low-high-carry of `type Base::uint_t`, s.t.:

     `[l0l1, h0h1]` ---becomes--> '[l0, h0+l1, h1+ov]`
                                             <---/
*/
template<class Base>
__device__ void
combine1(typename Base::ubig_t* lh, typename Base::uint_t* lhc) {
    using uint_t = typename Base::uint_t;
    lhc[0] = (uint_t) lh[0];
    uint_t h = (uint_t) lh[1];
    lhc[1] = h + ((uint_t) (lh[0] >> Base::bits));
    lhc[2] = ((uint_t) (lh[1] >> Base::bits)) + (lhc[1] < h);
}

/* This function takes two low and high parts of two consecutive multiplication convolutions, both
   stored as the data type `Base::ubig_t`, and it then first converts each set of low-high parts to
   a set of low-high-carry parts in the normal base type `Base::uint_t`, and then combines both
   sets. Below is a illustration of the process (where overflows denoted by `ov` and an arrow):

     Step 1.                                        <----\
      `[l00l01, h00h01]` --becomes--> `[l00, h00+l01, h01+ov]` --renamed--> `[l0, h0, c0]'
      `[l10l11, h10h11]` --becomes--> `[l10, h10+l11, h11+ov]` --renamed--> `[l1, h1, c1]'
                                                    <----/
     Step 2.               <---\
      '[l0, h0+l1, c0+h1+ov, c1+ov]`
                 <------/
*/
template<class Base>
__device__ void
combine2(typename Base::ubig_t* lh0, typename Base::ubig_t* lh1, typename Base::uint_t* lhc) {
    using uint_t = typename Base::uint_t;

    uint_t h0t = (uint_t) lh1[1];
    uint_t h0  = h0t + ((uint_t) (lh0[0] >> Base::bits));
    uint_t c0  = ((uint_t) (lh0[1] >> Base::bits)) + (h0 < h0t);

    uint_t h1t = (uint_t) lh1[1];
    uint_t h1  = h1t + ((uint_t) (lh1[0] >> Base::bits));
    uint_t c1  = ((uint_t) (lh1[1] >> Base::bits)) + (h1 < h1t);

    lhc[0] = (uint_t) lh0[0];
    lhc[1] = h0 + ((uint_t) lh1[0]);
    lhc[2] = c0 + h1 + (lhc[1] < h0);
    lhc[3] = c1 + (lhc[2] < h1);
}

// Computes a convolution iteration using `Base::ubig_t`-representation.
template<class Base>
__device__ void
iterate(typename Base::uint_t a, typename Base::uint_t b, typename Base::ubig_t* lh) {
    using ubig_t = typename Base::ubig_t;
    ubig_t ab = ((ubig_t) a) * ((ubig_t) b);
    lh[0] += ab & ((ubig_t) Base::HIGHEST);
    lh[1] += ab >> Base::bits;
}

/****************************************************/
/*** Big Integer Multiplication By Convolution v1 ***/
/****************************************************/

/** Each block multiplies one instance of big integers and each thread handle two units **/

template<class Base, uint32_t m>
__device__ void // ASSUMPTION: Shared memory buffer can store atleast `3*m` elements.
convMult1Run(typename Base::uint_t* shmem_as, typename Base::uint_t* shmem_bs,
             typename Base::uint_t* shmem_buff, typename Base::uint_t* rss) {
    using uint_t  = typename Base::uint_t;
    using ubig_t  = typename Base::ubig_t;
    using carry_t = typename Base::carry_t;

    // 1. compute low, high and carry (high overflow) for k1 and k2 (thread elements)
    uint_t lss[2]; lss[0] = 0; lss[1] = 0;
    uint_t hss[2]; hss[0] = 0; hss[1] = 0;
    uint_t css[2]; css[0] = 0; css[1] = 0;

    uint64_t k1 = threadIdx.x;
    for (int i=0; i<=k1; i++) {
        // compute high and low part of result
        int j  = k1 - i;
        ubig_t r  = ((ubig_t) shmem_as[i]) * ((ubig_t) shmem_bs[j]);
        uint_t lr = (uint_t) r;
        uint_t hr = (uint_t) (r >> Base::bits);
        // update l, h and c
        lss[0] += lr;
        hss[0] += hr + (lss[0] < lr);
        css[0] += hss[0] < hr;
    }

    uint64_t k2 = m-1 - k1;
    for (int i=0; i<=k2; i++) {
        // compute high and low part of result
        int j  = k2 - i;
        ubig_t r  = ((ubig_t) shmem_as[i]) * ((ubig_t) shmem_bs[j]);
        uint_t lr = (uint_t) r;
        uint_t hr = (uint_t) (r >> Base::bits);
        // update l, h, and c
        lss[1] += lr;
        hss[1] += hr + (lss[1] < lr);
        css[1] += hss[1] < hr;
    }
    __syncthreads();

    // 2. write low part, high part and carry part to shared memory
    shmem_buff[k1] = lss[0]; shmem_buff[k1+m] = hss[0]; shmem_buff[k1+2*m] = css[0];
    shmem_buff[k2] = lss[1]; shmem_buff[k2+m] = hss[1]; shmem_buff[k2+2*m] = css[1];
    __syncthreads();

    // 3. fetch low, high and carry from shared memory to registers
    lss[0] = shmem_buff[threadIdx.x*2];
    lss[1] = shmem_buff[threadIdx.x*2 + 1];
    hss[0] = (threadIdx.x) ? shmem_buff[threadIdx.x*2 + m - 1] : 0;
    hss[1] = shmem_buff[threadIdx.x*2 + m];
    css[0] = (threadIdx.x) ? shmem_buff[threadIdx.x*2 + 2*m - 2] : 0;
    css[1] = (threadIdx.x) ? shmem_buff[threadIdx.x*2 + 2*m - 1] : 0;
    __syncthreads();

    // 4. add low, high and carry
    baddKer2Run<Base,m,2>(lss, hss, rss, (carry_t*) shmem_buff);
    __syncthreads();
    baddKer2Run<Base,m,2>(css, rss, rss, (carry_t*) shmem_buff);
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
    convMult1Run<Base,m>(shmem, shmem+m, shmem, rss);
    __syncthreads();

    // 3. write result to global memory
    cpReg2Shm2Glb<uint_t,m,2,1>(rss, shmem, rs);
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

    // 2. multiply as and bs, then, multiply the result by a p times, i.e. a^p * b
    uint_t rss[2];
    convMult1Run<Base,m>(shmem, shmem+m, shmem+2*m, rss);
    __syncthreads();
    for(int i=1; i<p; i++) {
        cpReg2Shm<uint_t,2>(rss, shmem+m);
        __syncthreads();
        convMult1Run<Base,m>(shmem, shmem+m, shmem+2*m, rss);
        __syncthreads();
    }

    // 3. write result to global memory
    cpReg2Shm2Glb<uint_t,m,2,1>(rss, shmem, rs);
}

/****************************************************/
/*** Big Integer Multiplication By Convolution v2 ***/
/****************************************************/

/** Each block multiplies one instance of big integers and each thread handle two units **/

template<class Base, uint32_t m>
__device__ void // ASSUMPTION: Shared memory buffer can store atleast `3*m` elements.
convMult2Run(typename Base::uint_t* shmem_as, typename Base::uint_t* shmem_bs,
             typename Base::uint_t* shmem_buff, typename Base::uint_t* rss) {
    using uint_t  = typename Base::uint_t;
    using ubig_t  = typename Base::ubig_t;
    using carry_t = typename Base::carry_t;

    // 1. compute low, high and carry (high overflow) for k1 and k2 (thread elements)
    uint_t lss[2]; uint_t hss[2]; uint_t css[2];

    //   1.1 compute low-high parts of sum over `k1`
    ubig_t lh[2]; lh[0] = 0; lh[1] = 0;
    uint64_t k1 = threadIdx.x;
    for (int i=0; i<=k1; i++) {
        int j  = k1 - i;
        iterate<Base>(shmem_as[i], shmem_bs[j], lh);
    }

    //   1.2 combine low-high parts associated with `k1`
    uint_t lhc[3];
    combine1<Base>(lh, lhc);
    lss[0] = lhc[0]; hss[0] = lhc[1]; css[0] = lhc[2];

    //   1.3 compute low-high parts of sum over `k2`
    lh[0] = 0; lh[1] = 0;
    uint64_t k2 = m-1 - k1;
    for (int i=0; i<=k2; i++) {
        int j  = k2 - i;
        iterate<Base>(shmem_as[i], shmem_bs[j], lh);
    }

    //   1.4 combine low-high parts associated with `k2`
    combine1<Base>(lh, lhc);
    lss[1] = lhc[0]; hss[1] = lhc[1]; css[1] = lhc[2];
    __syncthreads();

    // 2. write low part, high part and carry part to shared memory
    shmem_buff[k1] = lss[0]; shmem_buff[k1+m] = hss[0]; shmem_buff[k1+2*m] = css[0];
    shmem_buff[k2] = lss[1]; shmem_buff[k2+m] = hss[1]; shmem_buff[k2+2*m] = css[1];
    __syncthreads();

    // 3. fetch low, high and carry from shared memory to registers
    lss[0] = shmem_buff[threadIdx.x*2];
    lss[1] = shmem_buff[threadIdx.x*2 + 1];
    hss[0] = (threadIdx.x) ? shmem_buff[threadIdx.x*2 + m - 1] : 0;
    hss[1] = shmem_buff[threadIdx.x*2 + m];
    css[0] = (threadIdx.x) ? shmem_buff[threadIdx.x*2 + 2*m - 2] : 0;
    css[1] = (threadIdx.x) ? shmem_buff[threadIdx.x*2 + 2*m - 1] : 0;
    __syncthreads();

    // 4. add low, high and carry
    baddKer2Run<Base,m,2>(lss, hss, rss, (carry_t*) shmem_buff);
    __syncthreads();
    baddKer2Run<Base,m,2>(css, rss, rss, (carry_t*) shmem_buff);
}

template<class Base, uint32_t m>
__global__ void
convMult2(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy as and bs into shared memory
    __shared__ uint_t shmem[m*3];
    cpGlb2Shm<uint_t,m,2,1>(as, shmem);
    cpGlb2Shm<uint_t,m,2,1>(bs, shmem+m);
    __syncthreads();

    // 2. multiply as and bs
    uint_t rss[2];
    convMult2Run<Base,m>(shmem, shmem+m, shmem, rss);
    __syncthreads();

    // 3. write result to global memory
    cpReg2Shm2Glb<uint_t,m,2,1>(rss, shmem, rs);
}

template<class Base, uint32_t m, uint32_t p> // runs p multiplications
__global__ void
convMult2Bench(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy as and bs into shared memory
    __shared__ uint_t shmem[m*5];
    cpGlb2Shm<uint_t,m,2,1>(as, shmem);
    cpGlb2Shm<uint_t,m,2,1>(bs, shmem+m);
    __syncthreads();

    // 2. multiply as and bs, then, multiply the result by a p times, i.e. a^p * b
    uint_t rss[2];
    convMult2Run<Base,m>(shmem, shmem+m, shmem+2*m, rss);
    __syncthreads();
    for(int i=1; i<p; i++) {
        cpReg2Shm<uint_t,2>(rss, shmem+m);
        __syncthreads();
        convMult2Run<Base,m>(shmem, shmem+m, shmem+2*m, rss);
        __syncthreads();
    }

    // 3. write result to global memory
    cpReg2Shm2Glb<uint_t,m,2,1>(rss, shmem, rs);
}

/****************************************************/
/*** Big Integer Multiplication By Convolution v3 ***/
/****************************************************/

/** Each block multiplies ipb instances of big integers and each thread handle q units **/

template<class Base, uint32_t m, uint32_t ipb>
__device__ void // ASSUMPTION: Shared memory buffer can store atleast `3*m` elements.
convMult3Run(typename Base::uint_t* shmem_as, typename Base::uint_t* shmem_bs,
             typename Base::uint_t* shmem_buff, typename Base::uint_t* rss) {
    using uint_t  = typename Base::uint_t;
    using ubig_t  = typename Base::ubig_t;
    using carry_t = typename Base::carry_t;

    // 1. compute low, high and carry (high overflow) for k1 and k2 (thread elements)
    uint_t lss[2]; uint_t hss[2]; uint_t css[2];

    //   1.1 compute low-high parts of sum over `k1`
    ubig_t lh[2]; lh[0] = 0; lh[1] = 0;
    uint64_t k1 = threadIdx.x;
    uint64_t k1_start = (k1/m)*m;
    for (int i=k1_start; i<=k1; i++) {
        int j  = k1 - i + k1_start;
        iterate<Base>(shmem_as[i], shmem_bs[j], lh);
    }

    //   1.2 combine low-high parts associated with `k1`
    uint_t lhc[3];
    combine1<Base>(lh, lhc);
    lss[0] = lhc[0]; hss[0] = lhc[1]; css[0] = lhc[2];

    //   1.3 compute low-high parts of sum over `k2`
    lh[0] = 0; lh[1] = 0;
    uint64_t k2 = ipb*m-1 - k1;
    uint64_t k2_start = (k2/m)*m;
    for (int i=k2_start; i<=k2; i++) {
        int j  = k2 - i + k2_start;
        iterate<Base>(shmem_as[i], shmem_bs[j], lh);
    }

    //   1.4 combine low-high parts associated with `k2`
    combine1<Base>(lh, lhc);
    lss[1] = lhc[0]; hss[1] = lhc[1]; css[1] = lhc[2];
    __syncthreads();

    // 2. write low part, high part and carry part to shared memory
    shmem_buff[k1] = lss[0]; shmem_buff[k1+m] = hss[0]; shmem_buff[k1+2*m] = css[0];
    shmem_buff[k2] = lss[1]; shmem_buff[k2+m] = hss[1]; shmem_buff[k2+2*m] = css[1];
    __syncthreads();

    // 3. fetch low, high and carry from shared memory to registers
    uint64_t off  = threadIdx.x*2;
    uint64_t ioff = off % m;
    lss[0] = shmem_buff[off];
    lss[1] = shmem_buff[off + 1];
    hss[0] = (ioff) ? shmem_buff[off + m - 1] : 0;
    hss[1] = shmem_buff[off + m];
    css[0] = (ioff) ? shmem_buff[off + 2*m - 2] : 0;
    css[1] = (ioff) ? shmem_buff[off + 2*m - 1] : 0;
    __syncthreads();

    // 4. add low, high and carry
    baddKer3Run<Base,m,2,ipb>(lss, hss, rss, (carry_t*) shmem_buff);
    __syncthreads();
    baddKer3Run<Base,m,2,ipb>(css, rss, rss, (carry_t*) shmem_buff);
}

template<class Base, uint32_t m, uint32_t ipb>
__global__ void
convMult3(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy as and bs into shared memory
    __shared__ uint_t shmem[ipb*m*3];
    cpGlb2Shm<uint_t,m,2,ipb>(as, shmem);
    cpGlb2Shm<uint_t,m,2,ipb>(bs, shmem+ipb*m);
    __syncthreads();

    // 2. multiply as and bs
    uint_t rss[2];
    convMult3Run<Base,m,ipb>(shmem, shmem+ipb*m, shmem, rss);
    __syncthreads();

    // 3. write result to global memory
    cpReg2Glb<uint_t,m,2,ipb>(rss, rs);
}

template<class Base, uint32_t m, uint32_t ipb, uint32_t p> // runs p multiplications
__global__ void
convMult3Bench(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy as and bs into shared memory
    __shared__ uint_t shmem[ipb*m*5];
    cpGlb2Shm<uint_t,m,2,ipb>(as, shmem);
    cpGlb2Shm<uint_t,m,2,ipb>(bs, shmem+ipb*m);
    __syncthreads();

    // 2. multiply as and bs, then, multiply the result by a p times, i.e. a^p * b
    uint_t rss[2];
    convMult3Run<Base,m,ipb>(shmem, shmem+ipb*m, shmem+2*ipb*m, rss);
    __syncthreads();
    for(int i=1; i<2; i++) {
        cpReg2Shm<uint_t,2>(rss, shmem+ipb*m);
        __syncthreads();
        convMult3Run<Base,m,ipb>(shmem, shmem+ipb*m, shmem+2*ipb*m, rss);
        __syncthreads();
    }

    // 3. write result to global memory
    cpReg2Glb<uint_t,m,2,ipb>(rss, rs);
}

/****************************************************/
/*** Big Integer Multiplication By Convolution v4 ***/
/****************************************************/

/** Each block multiplies one instance of big integers and each thread handle 4 units **/

template<class Base, uint32_t m>
__device__ void
convMult4Run(typename Base::uint_t* shmem_as,   typename Base::uint_t* shmem_bs,
             typename Base::uint_t* shmem_buff, typename Base::uint_t* rss) {
    using uint_t  = typename Base::uint_t;
    using ubig_t  = typename Base::ubig_t;
    using carry_t = typename Base::carry_t;

    // 1. compute low, high and carry (high overflow) for k1 and k2 (thread elements)
    uint_t lhck1[4];
    uint_t lhck2[4];
    {
        // 1.1. compute two consecutive convolutions over `k1`
        ubig_t lh0[2]; lh0[0] = 0; lh0[1] = 0;
        ubig_t lh1[2]; lh1[0] = 0; lh1[1] = 0;
        int k1 = threadIdx.x*2;

        for (int i=0; i<=k1; i++) {
            int j = k1 - i;
            uint_t a = shmem_as[i];
            iterate<Base>(a, shmem_bs[j], lh0);
            iterate<Base>(a, shmem_bs[j+1], lh1);
        }   iterate<Base>(shmem_as[k1+1], shmem_bs[0], lh1); // the remaining computation (`i=k1+1`)

        // 1.2. combine results associated with `k1` (i.e. compute `lhck1`)
        combine2<Base>(lh0, lh1, lhck1);

        // 1.3. compute two consecutive convolutions over `k2`
        lh0[0] = 0; lh0[1] = 0;
        lh1[0] = 0; lh1[1] = 0;
        int k2 = m-1 - k1;

        for (int i=0; i<=k2-1; i++) {
            int j = k2-1 - i;
            uint_t a = shmem_as[i];
            iterate<Base>(a, shmem_bs[j], lh0);
            iterate<Base>(a, shmem_bs[j+1], lh1);
        }   iterate<Base>(shmem_as[k2], shmem_bs[0], lh1); // the remaining computation (`i=k2`)

        // 1.4. combine results associated with `k2` (i.e. compute `lhck2`)
        combine2<Base>(lh0, lh1, lhck2);
    }
    __syncthreads();

    // 2. write low, high and carry parts to shared memory
    {
        /* Memory associated with `k1` (i.e. `lhck1`) is written in a straightforward coalesced
           fashion with stride `s`. However, for `k2` (i.e. `lhck2`) consecutive threads
           still access consecutive memory cells, but does so in reversed order.
           Hopefully, this also results in wrap-level memory hits.
        */
        int off = threadIdx.x;
        int s = m / 2;

        shmem_buff[off] = lhck1[0];
        shmem_buff[off+s] = lhck1[1];
        shmem_buff[off+2*s] = lhck1[2];
        shmem_buff[off+3*s] = lhck1[3];

        shmem_buff[s-off-1] = lhck2[0];
        shmem_buff[s*2-off-1] = lhck2[1];
        shmem_buff[s*3-off-1] = lhck2[2];
        shmem_buff[s*4-off-1] = lhck2[3];
    }
    __syncthreads();

    // 3. fetch low, high and carry parts from shared memory to registers
    {
        /* Memory cells are read in groups of two, where each group is fetched coalesced.
        */
        int off = threadIdx.x*2;
        int s = m / 2;

        lhck1[0] = shmem_buff[off];
        lhck2[2] = shmem_buff[off+1];

        lhck1[1] = shmem_buff[off+s];
        lhck2[3] = shmem_buff[off+s+1];

        lhck2[0] = (off) ? shmem_buff[off+2*s-1] : 0;
        lhck1[2] = shmem_buff[off+2*s];

        lhck2[1] = (off) ? shmem_buff[off+3*s-1] : 0;
        lhck1[3] = shmem_buff[off+3*s];
    }
    __syncthreads();

    // 4. add the (remaining) low, high and carry parts
    baddKer2Run<Base,m,4>(lhck1, lhck2, rss, (carry_t*) shmem_buff);
}

template<class Base, uint32_t m>
__global__ void
convMult4(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy as and bs into shared memory
    __shared__ uint_t shmem[m*2];
    cpGlb2Shm<uint_t,m,4,1>(as, shmem);
    cpGlb2Shm<uint_t,m,4,1>(bs, shmem+m);
    __syncthreads();

    // 2. multiply as and bs
    uint_t rss[4];
    convMult4Run<Base,m>(shmem, shmem+m, shmem, rss);
    __syncthreads();

    // 3. write result to global memory
    cpReg2Shm2Glb<uint_t,m,4,1>(rss, shmem, rs);
}

template<class Base, uint32_t m, uint32_t p> // runs p multiplications
__global__ void
convMult4Bench(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy as and bs into shared memory
    __shared__ uint_t shmem[m*4];
    cpGlb2Shm<uint_t,m,4,1>(as, shmem);
    cpGlb2Shm<uint_t,m,4,1>(bs, shmem+m);
    __syncthreads();

    // 2. multiply as and bs, then, multiply the result by a p times, i.e. a^p * b
    uint_t rss[4];
    convMult4Run<Base,m>(shmem,shmem+m,shmem+2*m,rss);
    __syncthreads();
    for(int i=1; i<p; i++) {
        cpReg2Shm<uint_t,4>(rss, shmem+m);
        __syncthreads();
        convMult4Run<Base,m>(shmem,shmem+m,shmem+2*m,rss);
        __syncthreads();
    }

    // 3. write result to global memory
    cpReg2Shm2Glb<uint_t,m,4,1>(rss, shmem, rs);
}

/****************************************************/
/*** Big Integer Multiplication By Convolution v5 ***/
/****************************************************/

/** Each block multiplies multiple instance of big integers and each thread handle 4 units **/

template<class Base, uint32_t m, uint32_t ipb>
__device__ void
convMult5Run(typename Base::uint_t* shmem_as,   typename Base::uint_t* shmem_bs,
             typename Base::uint_t* shmem_buff, typename Base::uint_t* rss) {
    using uint_t  = typename Base::uint_t;
    using ubig_t  = typename Base::ubig_t;
    using carry_t = typename Base::carry_t;

    // 1. compute low, high and carry (high overflow) for k1 and k2 (thread elements)
    uint_t lhck1[4];
    uint_t lhck2[4];
    {
        // 1.1. compute two consecutive sums over `k1`
        ubig_t lh0[2]; lh0[0] = 0; lh0[1] = 0;
        ubig_t lh1[2]; lh1[0] = 0; lh1[1] = 0;
        int k1 = threadIdx.x*2;
        int k1_start = (k1/m) * m;

        for (int i=k1_start; i<=k1; i++) {
            int j = k1 - i + k1_start;
            uint_t a = shmem_as[i];
            iterate<Base>(a, shmem_bs[j], lh0);
            iterate<Base>(a, shmem_bs[j+1], lh1);
        }   iterate<Base>(shmem_as[k1+1], shmem_bs[k1_start], lh1); // the remaining computation

        // 1.2. combine results associated with `k1` (i.e. compute `lhck1`)
        combine2<Base>(lh0, lh1, lhck1);

        // 1.3. compute two consecutive sums over `k2`
        lh0[0] = 0; lh0[1] = 0;
        lh1[0] = 0; lh1[1] = 0;
        int k2 = ipb*m-1 - k1;
        int k2_start = (k2/m) * m;

        for (int i=k2_start; i<=k2-1; i++) {
            // fetch memory
            int j = k2-1 - i + k2_start;
            uint_t a = shmem_as[i];
            iterate<Base>(a, shmem_bs[j], lh0);
            iterate<Base>(a, shmem_bs[j+1], lh1);
        }   iterate<Base>(shmem_as[k2], shmem_bs[k2_start], lh1); // the remaining computation

        // 1.4. combine results associated with `k2` (i.e. compute `lhck2`)
        combine2<Base>(lh0, lh1, lhck2);
    }
    __syncthreads();

    // 2. write low, high and carry parts to shared memory
    {
        /* Memory associated with `k1` (i.e. `lhck1`) is written in a straightforward coalesced
           fashion with stride `s`. However, for `k2` (i.e. `lhck2`) consecutive threads
           still access consecutive memory cells, but does so in reversed order.
           Hopefully, this also results in wrap-level memory hits.
        */
        int off = threadIdx.x;
        int s = ipb * m/2;

        shmem_buff[off] = lhck1[0];
        shmem_buff[off+s] = lhck1[1];
        shmem_buff[off+2*s] = lhck1[2];
        shmem_buff[off+3*s] = lhck1[3];

        shmem_buff[s-off-1] = lhck2[0];
        shmem_buff[s*2-off-1] = lhck2[1];
        shmem_buff[s*3-off-1] = lhck2[2];
        shmem_buff[s*4-off-1] = lhck2[3];
    }
    __syncthreads();

    // 3. fetch low, high and carry parts from shared memory to registers
    {
        /* Memory cells are read in groups of two, where each group is fetched coalesced.
        */
        int off = threadIdx.x*2;
        int s = ipb * m/2;

        lhck1[0] = shmem_buff[off];
        lhck2[2] = shmem_buff[off+1];

        lhck1[1] = shmem_buff[off+s];
        lhck2[3] = shmem_buff[off+s+1];

        lhck2[0] = (off*2 % m) ? shmem_buff[off+2*s-1] : 0;
        lhck1[2] = shmem_buff[off+2*s];

        lhck2[1] = (off*2 % m) ? shmem_buff[off+3*s-1] : 0;
        lhck1[3] = shmem_buff[off+3*s];
    }
    __syncthreads();

    // 4. add the (remaining) low, high and carry parts
    baddKer3Run<Base,m,4,ipb>(lhck1, lhck2, rss, (carry_t*) shmem_buff);
}

template<class Base, uint32_t m, uint32_t ipb>
__global__ void
convMult5(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy as and bs into shared memory
    __shared__ uint_t shmem[ipb*m*2];
    cpGlb2Shm<uint_t,m,4,ipb>(as, shmem);
    cpGlb2Shm<uint_t,m,4,ipb>(bs, shmem+ipb*m);
    __syncthreads();

    // 2. multiply as and bs
    uint_t rss[4];
    convMult5Run<Base,m,ipb>(shmem, shmem+ipb*m, shmem, rss);
    __syncthreads();

    // 3. write result to global memory
    cpReg2Shm2Glb<uint_t,m,4,ipb>(rss, shmem, rs);
}

template<class Base, uint32_t m, uint32_t ipb, uint32_t p> // runs p multiplications
__global__ void
convMult5Bench(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy as and bs into shared memory
    __shared__ uint_t shmem[ipb*m*4];
    cpGlb2Shm<uint_t,m,4,ipb>(as, shmem);
    cpGlb2Shm<uint_t,m,4,ipb>(bs, shmem+ipb*m);
    __syncthreads();

    // 2. multiply as and bs, then, multiply the result by a p times, i.e. a^p * b
    uint_t rss[4];
    convMult5Run<Base,m,ipb>(shmem,shmem+ipb*m,shmem+2*ipb*m,rss);
    __syncthreads();
    for(int i=1; i<p; i++) {
        cpReg2Shm<uint_t,4>(rss, shmem+ipb*m);
        __syncthreads();
        convMult5Run<Base,m,ipb>(shmem,shmem+ipb*m,shmem+2*ipb*m,rss);
        __syncthreads();
    }

    // 3. write result to global memory
    cpReg2Shm2Glb<uint_t,m,4,ipb>(rss, shmem, rs);
}

#endif // KERNEL_MULTIPLICATION



// DISCARDED BECAUSE SLOOOOW

// template<class Base, uint32_t m, uint32_t q>
// __device__ void
// convMult2Run(typename Base::uint_t* shmem_as, typename Base::uint_t* shmem_bs,
//              typename Base::uint_t* shmem_ls, typename Base::uint_t* shmem_hs,
//              typename Base::uint_t* shmem_cs, typename Base::carry_t* shmem_add,
//              typename Base::uint_t* rss) {
//     using uint_t  = typename Base::uint_t;
//     using ubig_t  = typename Base::ubig_t;
//     using carry_t = typename Base::carry_t;

//     // 1. compute low, high and carry (high overflow) for k1 and k2 (thread elements)
//     uint_t lss[q]; uint_t hss[q]; uint_t css[q];
//     for (int i=0; i<q; i++) {
//         lss[i] = 0; hss[i] = 0; css[i] = 0;
//     }

//     for (int s=0; s<q/2; s++) {
//         uint64_t k1 = threadIdx.x*(q/2) + s;
//         for (int i=0; i<=k1; i++) {
//             // compute high and low part of result
//             int j  = k1 - i;
//             ubig_t r  = ((ubig_t) shmem_as[i]) * ((ubig_t) shmem_bs[j]);
//             uint_t lr = (uint_t) r;
//             uint_t hr = (uint_t) (r >> Base::bits);
//             // update l, h and c
//             lss[s*2] += lr;
//             hss[s*2] += hr + (lss[s*2] < lr);
//             css[s*2] += hss[s*2] < hr;
//         }

//         uint64_t k2 = m-1 - k1;
//         for (int i=0; i<=k2; i++) {
//             // compute high and low part of result
//             int j  = k2 - i;
//             ubig_t r  = ((ubig_t) shmem_as[i]) * ((ubig_t) shmem_bs[j]);
//             uint_t lr = (uint_t) r;
//             uint_t hr = (uint_t) (r >> Base::bits);
//             // update l, h, and c
//             lss[s*2+1] += lr;
//             hss[s*2+1] += hr + (lss[s*2+1] < lr);
//             css[s*2+1] += hss[s*2+1] < hr;
//         }
//     }
//     __syncthreads();

//     // 2. write low part, high part and carry part to shared memory
//     for (int s=0; s<q/2; s++) {
//         uint64_t k1 = threadIdx.x*(q/2) + s;
//         uint64_t k2 = m-1 - k1;

//         shmem_ls[k1] = lss[s*2]; shmem_hs[k1] = hss[s*2]; shmem_cs[k1] = css[s*2];
//         shmem_ls[k2] = lss[s*2+1]; shmem_hs[k2] = hss[s*2+1]; shmem_cs[k2] = css[s*2+1];
//     }
//     __syncthreads();

//     // 3. fetch low, high and carry from shared memory to registers
//     for (int i=0; i<q; i++) {
//         uint64_t off = threadIdx.x*q + i;
//         lss[i] = shmem_ls[off];
//         hss[i] = (off)     ? shmem_hs[off-1] : 0;
//         css[i] = (off > 1) ? shmem_cs[off-2] : 0;
//     }
//     __syncthreads();

//     // 4. add low, high and carry
//     baddKer2Run<Base,m,q>(lss, hss, rss, shmem_add);
//     __syncthreads();
//     baddKer2Run<Base,m,q>(css, rss, rss, shmem_add);
// }

// template<class Base, uint32_t m, uint32_t q>
// __global__ void
// convMult2(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
//     using uint_t  = typename Base::uint_t;
//     using carry_t = typename Base::carry_t;

//     // 1. copy as and bs into shared memory
//     __shared__ uint_t shmem[m*3];
//     cpGlb2Shm<uint_t,m,q,1>(as, shmem);
//     cpGlb2Shm<uint_t,m,q,1>(bs, shmem+m);
//     __syncthreads();

//     // 2. multiply as and bs
//     uint_t rss[q];
//     convMult2Run<Base,m,q>(shmem, shmem+m, shmem, shmem+m, shmem+2*m, (carry_t*)shmem, rss);
//     __syncthreads();

//     // 3. write result to global memory
//     cpReg2Glb<uint_t,m,q,1>(rss, rs);
// }

// template<class Base, uint32_t m, uint32_t q, uint32_t p> // runs p multiplications
// __global__ void
// convMult2Bench(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
//     using uint_t  = typename Base::uint_t;
//     using carry_t = typename Base::carry_t;

//     // 1. copy as and bs into shared memory
//     __shared__ uint_t shmem[m*5];
//     cpGlb2Shm<uint_t,m,q,1>(as, shmem);
//     cpGlb2Shm<uint_t,m,q,1>(bs, shmem+m);
//     __syncthreads();

//     // 2. multiply as and bs, then, multiply the result by a p times, i.e. a^p * b
//     uint_t rss[q];
//     convMult2Run<Base,m,q>(shmem,shmem+m,shmem+2*m,shmem+3*m,shmem+4*m,(carry_t*)shmem+2*m,rss);
//     __syncthreads();
//     for(int i=1; i<p; i++) {
//         cpReg2Shm<uint_t,q>(rss, shmem+m);
//         __syncthreads();
//         convMult2Run<Base,m,q>(shmem,shmem+m,shmem+2*m,shmem+3*m,shmem+4*m,(carry_t*)shmem+2*m,rss);
//         __syncthreads();
//     }

//     // 3. write result to global memory
//     cpReg2Glb<uint_t,m,q,1>(rss, rs);
// }
