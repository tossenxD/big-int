#ifndef KERNELS
#define KERNELS

#include "ker-helpers.cu.h"

/*****************/
/*** Operators ***/
/*****************/

template<class Base>
class CarryProp {
    using carry_t = typename Base::carry_t;
public:
    typedef carry_t InpElTp;
    typedef carry_t RedElTp;
    static const bool commutative = true;
    static __device__ __host__ inline carry_t identInp()               { return 2; }
    static __device__ __host__ inline carry_t mapFun(const carry_t& c) { return c; }
    static __device__ __host__ inline carry_t identity()               { return 2; }
    static __device__ __host__ inline carry_t apply(const carry_t c1, const carry_t c2) {
        return (c1 & c2 & 2) | (((c1 & (c2 >> 1)) | c2) & 1);
    }
    static __device__ __host__ inline bool equals(const carry_t c1, const carry_t c2) {
        return (c1 == c2);
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
    typedef carry_t InpElTp;
    typedef carry_t RedElTp;
    static const bool commutative = true;
    static __device__ __host__ inline carry_t identInp()               { return 2; }
    static __device__ __host__ inline carry_t mapFun(const carry_t& c) { return c; }
    static __device__ __host__ inline carry_t identity()               { return 2; }
    static __device__ __host__ inline carry_t apply(const carry_t c1, const carry_t c2) {
        carry_t v1 = c1 & 3; carry_t f1 = c1 & 4;
        carry_t v2 = c2 & 3; carry_t f2 = c2 & 4;
        carry_t vr = f2 ? v2 : CarryProp<Base>::apply(v1, v2);
        return vr | f1 | f2;
    }
    static __device__ __host__ inline bool equals(const carry_t c1, const carry_t c2) {
        return (c1 == c2);
    }
    static __device__ __host__ inline carry_t remVolatile(volatile carry_t& c) {
        carry_t res = c;
        return res;
    }
};

/*******************************/
/*** Big Integer Addition v1 ***/
/*******************************/

// Each block adds one instance of big integers and each thread adds one unit
template<class Base, uint32_t m>
__global__ void baddKer1(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;
    __shared__ carry_t shmem[m];

    // 1. fetch units from global memory
    uint64_t gid = blockIdx.x * m + threadIdx.x;
    uint_t  a = as[gid];
    uint_t  b = bs[gid];

    // 2. compute result and carry
    uint_t  r = a + b;
    carry_t c = ((carry_t) (r < a)) | (((carry_t) (r == Base::HIGHEST)) << 1);
    shmem[threadIdx.x] = c;
    __syncthreads();

    // 3. propagate carries by exclusive scan
    c = scanIncBlock< CarryProp<Base> >(shmem, threadIdx.x);
    __syncthreads();
    shmem[threadIdx.x] = CarryProp<Base>::identity();
    __syncthreads();
    if(threadIdx.x < m-1)
        shmem[threadIdx.x+1] = c;
    __syncthreads();
    c = shmem[threadIdx.x];

    // 4. write result to global memory
    rs[gid] = r + (c & 1);    
}

/*******************************/
/*** Big Integer Addition v2 ***/
/*******************************/

// Each block adds one instance of big integers and each thread adds multiple units
template<class Base, uint32_t m, uint32_t q>
__global__ void baddKer2(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t  = typename Base::uint_t;
    using carry_t = typename Base::carry_t;
    __shared__ carry_t shmem[m];
    uint_t  ass[q];
    uint_t  bss[q];
    uint_t  rss[q];
    carry_t css[q];

    // 1. fetch units from global memory
    cpGlb2Reg<uint_t,m,q,1>(as, ass);
    __syncthreads();
    cpGlb2Reg<uint_t,m,q,1>(bs, bss);
    __syncthreads();

    // 2. compute result, carry and thread-level scan
    carry_t acc = CarryProp<Base>::identity();
    for(int i=0; i<q; i++) {
        rss[i] = ass[i] + bss[i];
        css[i] = ((carry_t) (rss[i] < ass[i])) | (((carry_t) (rss[i] == Base::HIGHEST)) << 1);
        acc = CarryProp<Base>::apply(acc, css[i]);
    }
    shmem[threadIdx.x] = acc;
    __syncthreads();

    // 3. propagate carries and write them to shared memory
    acc = scanIncBlock< CarryProp<Base> >(shmem, threadIdx.x);
    __syncthreads();
    shmem[threadIdx.x] = CarryProp<Base>::identity();
    if(threadIdx.x < m-1)
        shmem[threadIdx.x+1] = acc;
    __syncthreads();

    // 4. write result to global memory
    acc = shmem[threadIdx.x];
    for(int i=0; i<q; i++) {
        rss[i] += acc & 1;
        acc = CarryProp<Base>::apply(acc, css[i]);
    }
    cpReg2Glb<uint_t,m,q,1>(rss, rs);
}

/*******************************/
/*** Big Integer Addition v3 ***/
/*******************************/

// Each block adds multiple instances of big integers and each thread adds multiple units
template<class Base, uint32_t m, uint32_t q, uint32_t ipb>
__global__ void baddKer3(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t = typename Base::uint_t;
    using carry_t = typename Base::carry_t;
    __shared__ carry_t shmem[m*ipb];
    uint_t ass[q];
    uint_t bss[q];
    uint_t rss[q];
    uint_t css[q];

    // 1. copy from global memory to registers
    cpGlb2Reg<uint_t,m,q,ipb>(as, ass);
    __syncthreads();
    cpGlb2Reg<uint_t,m,q,ipb>(bs, bss);
    __syncthreads();
    
    // 2. compute result, carry and thread-level scan
    carry_t acc = SegCarryProp<Base>::identity();
    for(int i=0; i<q; i++) {
        rss[i] = ass[i] + bss[i];
        css[i] = ((carry_t) (rss[i] < ass[i])) | (((carry_t) (rss[i] == Base::HIGHEST)) << 1);
        acc = SegCarryProp<Base>::apply(acc, css[i]);
    }
    acc += (threadIdx.x % (m/q) == 0) ? 4 : 0;
    shmem[threadIdx.x] = acc;
    __syncthreads();

    // 3. propagate carries and write them to shared memory
    acc = scanIncBlock< SegCarryProp<Base> >(shmem, threadIdx.x);
    __syncthreads();
    shmem[threadIdx.x] = acc;
    __syncthreads();
    acc = (threadIdx.x % (m/q) == 0) ? SegCarryProp<Base>::identity() : shmem[threadIdx.x-1];
    __syncthreads();

    // 4. write results to global memory
    for(int i=0; i<q; i++) {
        rss[i] += (acc & 1);
        acc = SegCarryProp<Base>::apply(acc, css[i]);
    }
    cpReg2Glb<uint_t,m,q,ipb>(rss, rs);
}

#endif // BIG_INT_KERNELS
