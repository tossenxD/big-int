#ifndef KERNEL_HELPERS
#define KERNEL_HELPERS

#define WARP   (32)
#define lgWARP  (5)

#define HIGHEST32 ( 0xFFFFFFFF )
#define HIGHEST64 ( 0xFFFFFFFFFFFFFFFF )

/***********************/
/*** Data Structures ***/
/***********************/

typedef unsigned __int128 uint128_t;

struct U64bits {
    using uint_t = uint64_t;
    using sint_t = int64_t;
    using ubig_t = uint128_t;
    using carry_t= uint32_t;
    static const int32_t  bits = 64;
    static const uint_t HIGHEST = HIGHEST64;
};

struct U32bits {
    using uint_t = uint32_t;
    using sint_t = int32_t;
    using ubig_t = uint64_t;
    using carry_t= uint32_t;
    static const int32_t  bits = 32;
    static const uint_t HIGHEST = HIGHEST32;
};


/********************************/
/*** Parallel Building Blocks ***/
/********************************/

template<class OP>
__device__ inline typename OP::RedElTp
scanIncWarp(volatile typename OP::RedElTp* ptr, const unsigned int idx) {
    const unsigned int lane = idx & (WARP-1);
    #pragma unroll
    for(int d=0; d<lgWARP; d++) {
        int h = 1 << d;
        if (lane >= h)
            ptr[idx] = OP::apply(ptr[idx-h], ptr[idx]);
    }
    return OP::remVolatile(ptr[idx]);
}

template<class OP>
__device__ inline typename OP::RedElTp
scanIncBlock(volatile typename OP::RedElTp* ptr, const unsigned int idx) {
    const unsigned int lane   = idx & (WARP-1);
    const unsigned int warpid = idx >> lgWARP;

    // 1. perform scan at warp level
    typename OP::RedElTp res = scanIncWarp<OP>(ptr,idx);
    __syncthreads();

    // 2. place the end-of-warp results in
    //   the first warp. This works because
    //   warp size = 32, and 
    //   max block size = 32^2 = 1024
    typename OP::RedElTp tmp = OP::remVolatile(ptr[idx]);
    __syncthreads();
    if (lane == (WARP-1))
        ptr[warpid] = tmp;
    __syncthreads();

    // 3. scan again the first warp
    if (warpid == 0) scanIncWarp<OP>(ptr, idx);
    __syncthreads();

    // 4. accumulate results from previous step;
    if (warpid > 0)
        res = OP::apply(ptr[warpid-1], res);

    return res;
}

template<class OP>
__device__ inline typename OP::RedElTp
scanExcBlock(volatile typename OP::RedElTp* ptr, const unsigned int idx) {
    // 1. perform inclusive scan of block
    typename OP::RedElTp tmp = scanIncBlock<OP>(ptr, idx);
    __syncthreads();

    // 2. return result of predecessor thread
    ptr[idx] = tmp;
    __syncthreads();
    return (idx == 0) ? OP::identity() : ptr[idx-1];
}


/***********************************************************/
/*** Remapping to/from Gobal, Shared and Register Memory ***/
/***********************************************************/

template<class S, uint32_t m, uint32_t q, uint32_t ipb>
__device__ inline
void cpGlb2Shm (S* glb, volatile S* shm, S reg[q]) {
    uint64_t glb_offs = blockIdx.x * (m * ipb);
    #pragma unroll
    for(int i=0; i<q; i++) {
        uint32_t loc_pos = i*(ipb*m/q) + threadIdx.x;
        shm[loc_pos] = glb[glb_offs + loc_pos];
    }
}

template<class S, uint32_t m, uint32_t q, uint32_t ipb>
__device__ inline
void cpShm2Glb (S reg[q], volatile S* shm, S* glb) { 
    uint64_t glb_offs = blockIdx.x * (ipb * m);
    #pragma unroll
    for(int i=0; i<q; i++) {
        uint32_t loc_pos = i*(ipb*m/q) + threadIdx.x;
        glb[glb_offs + loc_pos] = shm[loc_pos];
    }
}

template<class S, uint32_t q>
__device__ inline
void cpReg2Shm (S reg[q], volatile S* shm) {
    #pragma unroll
    for(int i=0; i<q; i++)
        shm[q*threadIdx.x + i] = reg[i];
}

template<class S, uint32_t q>
__device__ inline
void cpShm2Reg (volatile S* shm, S reg[q]) {
    #pragma unroll
    for(int i=0; i<q; i++)
        reg[i] = shm[q*threadIdx.x + i];
}

template<class S, uint32_t m, uint32_t q, uint32_t ipb>
__device__ inline
void cpGlb2Shm2Reg (S* glb, volatile S* shm, S reg[q]) {
    cpGlb2Shm<S,m,q,ipb>(glb, shm);
    __syncthreads();
    cpShm2Reg<S,m,q,ipb>(shm, reg);
}

template<class S, uint32_t m, uint32_t q, uint32_t ipb>
__device__ inline
void cpReg2Shm2Glb (S reg[q], volatile S* shm, S* glb) {
    cpReg2Shm<S,m,q,ipb>(reg, shm);
    __syncthreads();
    cpShm2Glb<S,m,q,ipb>(shm, glb);
}

template<class S, uint32_t m, uint32_t q, uint32_t ipb>
__device__ inline
void cpGlb2Reg (S* glb, S reg[q]) {
    uint64_t glb_offs = blockIdx.x * (m * ipb);
    #pragma unroll
    for(int i=0; i<q; i++)
        reg[i] = glb[glb_offs + threadIdx.x*q + i];
}

template<class S, uint32_t m, uint32_t q, uint32_t ipb>
__device__ inline
void cpReg2Glb (S reg[q], S* glb) { 
    uint64_t glb_offs = blockIdx.x * (ipb * m);
    #pragma unroll
    for(int i=0; i<q; i++)
        glb[glb_offs + threadIdx.x*q + i] = reg[i];
}

#endif //KERNEL_HELPERS
