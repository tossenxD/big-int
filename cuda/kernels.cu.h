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
    static __device__ __host__ inline carry_t identInp()                { return 2;  }
    static __device__ __host__ inline carry_t mapFun(const carry_t& el) { return el; }
    static __device__ __host__ inline carry_t identity()                { return 2;  }
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
    using carry_t = typename Base::carry_t; // TODO
public:
    typedef carry_t InpElTp;
    typedef carry_t RedElTp;
    static const bool commutative = true;
    static __device__ __host__ inline carry_t identInp()                { return 2;  }
    static __device__ __host__ inline carry_t mapFun(const carry_t& el) { return el; }
    static __device__ __host__ inline carry_t identity()                { return 2;  }
    static __device__ __host__ inline carry_t apply(const carry_t c1, const carry_t c2) {
        carry_t v1 = c1 & 3;
        carry_t v2 = c2 & 3;
        carry_t vr = (c1 & 4) ? v2 : ((v1 & v2 & 2) | (((v1 & (v2 >> 1)) | v2) & 1));
        return vr;//(c1 & c2 & 4) | vr;
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

// Each block adds one instance of big integers of and each thread adds one unit
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

// Each block adds one instance of big integers of and each thread adds one unit
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

//* Performs parallel addition of big integers;
//* `m` is size of the big integers in Base::uint_t units; `q` is the sequentialization factor;
//* `ipb` is number of instances of big integers per block.
template<class Base, uint32_t m, uint32_t q, uint32_t ipb>
__global__ void baddKer(typename Base::uint_t* as, typename Base::uint_t* bs, typename Base::uint_t* rs) {
    using uint_t = typename Base::uint_t;
    __shared__ uint_t shmem[m*ipb];
    uint_t ass[q];
    uint_t bss[q];
    uint_t rss[q];
    uint_t css[q];

    // copy from global memory to registers
    cpGlb2Reg<uint_t,m,q,ipb>(as, ass);
    __syncthreads();
    cpGlb2Reg<uint_t,m,q,ipb>(bs, bss);
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
        shmem[threadIdx.x] = last_carry;
        __syncthreads();
        scanIncBlock< SegCarryProp<Base> >(shmem, threadIdx.x);
        uint_t carry_prefix = (threadIdx.x % (m/q) == 0) ? SegCarryProp<Base>::identity() : shmem[threadIdx.x-1];
        __syncthreads();
        
        for(int i=0; i<q; i++) {
            rss[i] += (carry_prefix & 1);
            carry_prefix = SegCarryProp<Base>::apply(carry_prefix, css[i]);
        }

    }
#if 0
    // scan carries
    if (threadIdx.x % (m/q) == 0) css[q-1] += 4; // use third bit as flag
    cpReg2Shm<uint_t,q>(css, shmem);
    __syncthreads();
    scanExcBlockBlelloch<SegCarryProp<Base>,m,q>(shmem);
    cpShm2Reg<uint_t,q>(sh_mem, css);

    // update result from the propagated carries
    for(int i=0; i<q; i++) rss[i] += (css[i] & 1);
#endif
    cpReg2Glb<uint_t,m,q,ipb>(rss, rs);
}

#endif // BIG_INT_KERNELS
