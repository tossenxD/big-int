#ifndef KERNEL_ADDITION
#define KERNEL_ADDITION

#include "ker-helpers.cu.h"

/*****************/
/*** Operators ***/
/*****************/

template<class Base>
class CarryProp {
    using carry_t = typename Base::carry_t;
public:
    typedef carry_t RedElTp;
    static __device__ __host__ inline carry_t identity() { return 2; }
    static __device__ __host__ inline carry_t apply(const carry_t c1, const carry_t c2) {
        return (c1 & c2 & 2) | (((c1 & (c2 >> 1)) | c2) & 1);
    }
    static __device__ __host__ inline carry_t remVolatile(volatile carry_t& c) {
        carry_t res = c;
        return res;
    }
};

template<class Base>
class SegCarryProp {
    using carry_t = typename Base::carry_t;
public:
    typedef typename CarryProp<Base>::RedElTp RedElTp;
    static __device__ __host__ inline carry_t identity() { return CarryProp<Base>::identity(); }
    static __device__ __host__ inline carry_t setFlag(const carry_t c) { return c | 4; }
    static __device__ __host__ inline carry_t apply(const carry_t c1, const carry_t c2) {
        carry_t v1 = c1 & 3; carry_t f1 = c1 & 4;
        carry_t v2 = c2 & 3; carry_t f2 = c2 & 4;
        carry_t vr = f2 ? v2 : CarryProp<Base>::apply(v1, v2);
        return vr | f1 | f2;
    }
    static __device__ __host__ inline carry_t remVolatile(volatile carry_t& c) {
        return CarryProp<Base>::remVolatile(c);
    }
};


/*******************************/
/*** Big Integer Addition v1 ***/
/*******************************/

/** Each block adds one instance of big integers and each thread adds one unit **/

template<class Base, uint32_t m>
__device__ inline typename Base::uint_t
baddKer1Run(typename Base::uint_t a, typename Base::uint_t b, typename Base::carry_t* shmem) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;
    
    // 1. compute result and carry
    uint_t r = a + b;
    carry_t c = ((carry_t) (r < a)) | (((carry_t) (r == Base::HIGHEST)) << 1);
    shmem[threadIdx.x] = c;
    __syncthreads();

    // 2. propagate carries
    c = scanExcBlock< CarryProp<Base> >(shmem, threadIdx.x);

    // 3. add carries to result and return
    return r + (c & 1);
}

template<class Base, uint32_t m>
__global__ void
baddKer1(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. fetch units from global memory
    uint64_t gid = blockIdx.x * m + threadIdx.x;
    uint_t a = as[gid];
    uint_t b = bs[gid];

    // 2. compute results with carry propagation
    __shared__ carry_t shmem[m];
    uint_t r = baddKer1Run<Base,m>(a, b, shmem);

    // 3. write result to global memory
    rs[gid] = r;    
}

template<class Base, uint32_t m, uint32_t p> // runs p additions
__global__ void
baddKer1Bench(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. fetch units from global memory
    uint64_t gid = blockIdx.x * m + threadIdx.x;
    uint_t a = as[gid];
    uint_t b = bs[gid];

    // 2. compute results with carry propagation p times
    __shared__ carry_t shmem[m];
    uint_t r = b;
    #pragma unroll
    for(int i=0; i<p; i++) {
        r = baddKer1Run<Base,m>(a, r, shmem);
        __syncthreads();
    }

    // 3. write result to global memory
    rs[gid] = r;    
}


/*******************************/
/*** Big Integer Addition v2 ***/
/*******************************/

/** Each block adds one instance of big integers and each thread adds multiple units **/

template<class Base, uint32_t m, uint32_t q>
__device__ void
baddKer2Run(typename Base::uint_t* ass, typename Base::uint_t* bss,
            typename Base::uint_t* rss, typename Base::carry_t* shmem) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;
    
    // 1. compute result, carry and thread-level (register) scan
    carry_t css[q];
    carry_t acc = CarryProp<Base>::identity();
    #pragma unroll
    for(int i=0; i<q; i++) {
        rss[i] = ass[i] + bss[i];
        css[i] = ((carry_t) (rss[i] < ass[i])) | (((carry_t) (rss[i] == Base::HIGHEST)) << 1);
        acc = CarryProp<Base>::apply(acc, css[i]);
    }
    shmem[threadIdx.x] = acc;
    __syncthreads();

    // 2. propagate carries
    acc = scanExcBlock< CarryProp<Base> >(shmem, threadIdx.x);

    // 3. add carries to results
    #pragma unroll
    for(int i=0; i<q; i++) {
        rss[i] += acc & 1;
        acc = CarryProp<Base>::apply(acc, css[i]);
    }
}


template<class Base, uint32_t m, uint32_t q>
__global__ void
baddKer2(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. fetch units from global memory
    uint_t ass[q];
    uint_t bss[q];
    cpGlb2Reg<uint_t,m,q,1>(as, ass);
    cpGlb2Reg<uint_t,m,q,1>(bs, bss);
    __syncthreads();

    // 2. compute results with carry propagation
    uint_t rss[q];
    __shared__ carry_t shmem[m];
    baddKer2Run<Base,m,q>(ass, bss, rss, shmem);

    // 3. write result to global memory
    cpReg2Glb<uint_t,m,q,1>(rss, rs);
}


template<class Base, uint32_t m, uint32_t q, uint32_t p> // runs p additions
__global__ void
baddKer2Bench(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. fetch units from global memory
    uint_t ass[q];
    uint_t bss[q];
    cpGlb2Reg<uint_t,m,q,1>(as, ass);
    cpGlb2Reg<uint_t,m,q,1>(bs, bss);
    __syncthreads();

    // 2. compute results with carry propagation p times
    uint_t* rss = bss;
    __shared__ carry_t shmem[m];
    #pragma unroll
    for(int i=0; i<p; i++) {
        baddKer2Run<Base,m,q>(ass, rss, rss, shmem);
        __syncthreads();
    }

    // 3. write result to global memory
    cpReg2Glb<uint_t,m,q,1>(rss, rs);
}


/*******************************/
/*** Big Integer Addition v3 ***/
/*******************************/

/** Each block adds multiple instances of big integers and each thread adds multiple units **/

template<class Base, uint32_t m, uint32_t q, uint32_t ipb>
__device__ void
baddKer3Run(typename Base::uint_t* ass, typename Base::uint_t* bss,
            typename Base::uint_t* rss, typename Base::carry_t* shmem) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;
    const bool new_segm = threadIdx.x % (m/q) == 0;

    // 1. compute result, carry, register-level scan, and segment flags
    uint_t css[q];
    carry_t acc = new_segm ? SegCarryProp<Base>::setFlag( SegCarryProp<Base>::identity() )
                           : SegCarryProp<Base>::identity();
    #pragma unroll
    for(int i=0; i<q; i++) {
        rss[i] = ass[i] + bss[i];
        css[i] = ((carry_t) (rss[i] < ass[i])) | (((carry_t) (rss[i] == Base::HIGHEST)) << 1);
        acc = SegCarryProp<Base>::apply(acc, css[i]);
    }
    shmem[threadIdx.x] = acc;
    __syncthreads();

    // 2. propagate carries
    acc = scanExcBlock< SegCarryProp<Base> >(shmem, threadIdx.x);
    acc = new_segm ? SegCarryProp<Base>::identity() : acc;

    // 3. add carries to results
    #pragma unroll
    for(int i=0; i<q; i++) {
        rss[i] += (acc & 1);
        acc = SegCarryProp<Base>::apply(acc, css[i]);
    }
}


template<class Base, uint32_t m, uint32_t q, uint32_t ipb>
__global__ void
baddKer3(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy from global memory to registers
    uint_t ass[q];
    uint_t bss[q];
    cpGlb2Reg<uint_t,m,q,ipb>(as, ass);
    cpGlb2Reg<uint_t,m,q,ipb>(bs, bss);
    __syncthreads();

    // 2. compute results with carry propagation
    uint_t rss[q];
    __shared__ carry_t shmem[m*ipb];
    baddKer3Run<Base,m,q,ipb>(ass, bss, rss, shmem);

    // 3. write results to global memory
    cpReg2Glb<uint_t,m,q,ipb>(rss, rs);
}

template<class Base, uint32_t m, uint32_t q, uint32_t ipb, uint32_t p> // runs p additions
__global__ void
baddKer3Bench(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;

    // 1. copy from global memory to registers
    uint_t ass[q];
    uint_t bss[q];
    cpGlb2Reg<uint_t,m,q,ipb>(as, ass);
    cpGlb2Reg<uint_t,m,q,ipb>(bs, bss);
    __syncthreads();

    // 2. compute results with carry propagation p times
    uint_t* rss = bss;
    __shared__ carry_t shmem[m*ipb];
    #pragma unroll
    for(int i=0; i<p; i++) {
        baddKer3Run<Base,m,q,ipb>(ass, rss, rss, shmem);
        __syncthreads();
    }

    // 3. write results to global memory
    cpReg2Glb<uint_t,m,q,ipb>(rss, rs);
}

#endif // KERNEL_ADDITION
