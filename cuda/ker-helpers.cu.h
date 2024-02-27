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

template<class Base>
class CarryProp {
    using uint_t = typename Base::uint_t;
public:
    typedef uint_t InpElTp;
    typedef uint_t RedElTp;
    static const bool commutative = true;
    static __device__ __host__ inline uint_t identInp()               { return 2;  }
    static __device__ __host__ inline uint_t mapFun(const uint_t& el) { return el; }
    static __device__ __host__ inline uint_t identity()               { return 2;  }
    static __device__ __host__ inline uint_t apply(const uint_t c1, const uint_t c2) {
        return (c1 & c2 & 2) | (((c1 & (c2 >> 1)) | c2) & 1);
    }
    static __device__ __host__ inline bool equals(const uint_t c1, const uint_t c2) {
        return (c1 == c2);
    }
    static __device__ __host__ inline uint_t remVolatile(volatile uint_t& c) {
        uint_t res = c;
        return res;
    }
};

/***********************/
/*** Building Blocks ***/
/***********************/

template<class OP>
__device__ inline typename OP::RedElTp
scanIncWarp( volatile typename OP::RedElTp* ptr, const unsigned int idx ) {
#pragma unroll
    for(int d=0; d<lgWARP; d++) {
        int h = 1 << d;
        if ((idx & (WARP-1)) >= h) ptr[idx] = OP::apply(ptr[idx-h], ptr[idx]);
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
    if (lane == (WARP-1)) ptr[warpid] = tmp;
    __syncthreads();

    // 3. scan again the first warp
    if (warpid == 0) scanIncWarp<OP>(ptr, idx);
    __syncthreads();

    // 4. accumulate results from previous step;
    if (warpid > 0) res = OP::apply(ptr[warpid-1], res);
    return res;
}

template<class OP>
__device__ inline typename OP::RedElTp
scanExcBlock(volatile typename OP::RedElTp* ptr, const unsigned int idx) {
    typename OP::RedElTp res = scanIncBlock<OP>(ptr,idx);
    __syncthreads();
    ptr[idx] = res;
    res = OP::identity();
    __syncthreads();
    if (idx > 0) res = ptr[idx-1];
    return res;
}

/* Performs a blockwide exclusive scan in shared memory with sequentialization; */
/* `m` is blocksize and `q` is sequentialization factor. */
template<class OP, uint32_t m, uint32_t q>
__device__ inline typename OP::RedElTp
scanExcBlockBlelloch(volatile typename OP::RedElTp* ptr) {
    // upsweep
    for(int d=1; d<m; d*=2) {
        for(int i=0; i<q; i++) {
            uint32_t idx = i*(m/q) + threadIdx.x;
            if (idx%(2*d) == (2*d-1))
                ptr[idx] = OP::apply(ptr[idx-d], ptr[idx]);
        }
        __syncthreads();
    }
    
    // insert neutral element
    if (threadIdx.x == 0) ptr[m-1] = OP::identity();

    // downsweep
    for(int d=m/2; d>0; d/=2){
        for(int i=0; i<q; i++) {
            uint32_t idx = i*(m/q) + threadIdx.x;
            typename OP::RedElTp tmp = OP::identity();
            if (idx%(2*d) == (2*d-1)) tmp = ptr[idx-d];
            __syncthreads();
            if (idx%(2*d) == (2*d-1)) ptr[idx-d] = ptr[idx];
            ptr[idx] = OP::apply(ptr[idx],tmp);
        }
        __syncthreads();
    }
}

/***********************************************************/
/*** Remapping to/from Gobal, Shared and Register Memory ***/
/***********************************************************/

template<class S, uint32_t IPB, uint32_t M, uint32_t Q>
__device__ inline
void cpGlb2Sh ( S* ass, S* bss
              , S* Ash, S* Bsh 
) { 
    // 1. read from global to shared memory
    uint64_t glb_offs = blockIdx.x * (IPB * M);

    for(int i=0; i<Q; i++) {
        uint32_t loc_pos = i*(IPB*M/Q) + threadIdx.x;
        S tmp_a = 0, tmp_b = 0;
        //if(loc_pos < IPB*M) 
        {
            tmp_a = ass[glb_offs + loc_pos];
            tmp_b = bss[glb_offs + loc_pos];
        }
        Ash[loc_pos] = tmp_a;
        Bsh[loc_pos] = tmp_b;
    }
}

template<class S, uint32_t IPB, uint32_t M, uint32_t Q>
__device__ inline
void cpSh2Glb(S* Hsh, S* rss) { 
    // 3. write from shared to global memory
    uint64_t glb_offs = blockIdx.x * (IPB * M);

    for(int i=0; i<Q; i++) {
        uint32_t loc_pos = i*(IPB*M/Q) + threadIdx.x;
        //if(loc_pos < IPB*M) 
        {
            rss[glb_offs + loc_pos] = Hsh[loc_pos];
        }
    }
}

template<class S, uint32_t M, uint32_t Q>
__device__ inline
void cpGlb2Reg ( volatile S* shmem, S* ass, S Arg[Q] ) { 
    // 1. read from global to shared memory
    uint64_t glb_offs = blockIdx.x * M;

    for(int i=0; i<Q; i++) {
        uint32_t loc_pos = i*(M/Q) + threadIdx.x;
        S tmp_a = 0;
        {
            tmp_a = ass[glb_offs + loc_pos];
        }
        shmem[loc_pos] = tmp_a;
    }
    __syncthreads();
    // 2. read from shmem to regs
    for(int i=0; i<Q; i++) {
        Arg[i] = shmem[Q*threadIdx.x + i];
    }
}

template<class S, uint32_t M, uint32_t Q>
__device__ inline
void cpReg2Glb ( volatile S* shmem , S Rrg[Q], S* rss ) { 
    // 1. write from regs to shared memory
    for(int i=0; i<Q; i++) {
        shmem[Q*threadIdx.x + i] = Rrg[i];
    }
    __syncthreads();
    // 2. write from shmem to global
    uint64_t glb_offs = blockIdx.x * M;
    for(int i=0; i<Q; i++) {
        uint32_t loc_pos = i*(M/Q) + threadIdx.x;
        //if(loc_pos < IPB*M) 
        {
            rss[glb_offs + loc_pos] = shmem[loc_pos];
        }
    }
}

template<class S, uint32_t Q>
__device__ inline
void cpReg2Shm ( S Rrg[Q], volatile S* shmem ) { 
    for(int i=0; i<Q; i++) {
        shmem[Q*threadIdx.x + i] = Rrg[i];
    }
}

template<class S, uint32_t Q>
__device__ inline
void cpShm2Reg ( volatile S* shmem, S Rrg[Q] ) { 
    for(int i=0; i<Q; i++) {
        Rrg[i] = shmem[Q*threadIdx.x + i];
    }
}

#endif //KERNEL_HELPERS
