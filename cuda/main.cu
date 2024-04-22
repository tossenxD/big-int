#include "helpers.h"
#include "ker-add.cu.h"
#include "ker-mul.cu.h"

using namespace std;

#define GPU_RUNS_ADD      100
#define GPU_RUNS_MUL      25
#define VERSION           0

#define ADD               1
#define MUL               1
#define BASE32            0
#define BASE64            1
#define ADD_TEN           0
#define MUL_SIX           0
#define WITH_VALIDATION   0
#define PRINT_DEBUG_INFO  0
#define FULL_TEST_SUITE   0
#define FULLER_TEST_SUITE 0

/* This file contains different methods for running the big integer arithmetic CUDA kernels.

   The runtime input specifies the total work to be computed, i.e., how many bits total we run the
   kernels on. E.g. say we input `1024` as the total work and we run with 512-bit big integers
   in base 64-bits; then, the precision of the big integers are `512/64 = 8` and the number of
   big integers to be computed are `1024/512 = 2`, making the kernels run on a total of 1024-bits.

   The precision of the big integers are denoted by `m`, the base by `Base` (see data structures in
   file `ker-helpers.cu.h`), the number of big integer instances `num_instances` and the number of
   instances to be handled at block-level `ipb` (instances per block).

   While the total work is given as a runtime input, this file contains many other parameters that
   determine what kernels to run and how to run them. These can all be adjusted in the definitions
   above, according to the tables below.

   +----------------------------------------------------------------------------------------------+
   | Numeric parameters                                                                           |
   +=====================+========================================================================+
   | GPU_RUNS_ADD        | How many runs addition kernels are averaged over.                      |
   +---------------------+------------------------------------------------------------------------+
   | GPU_RUNS_MUL        | How many runs multiplication kernels are averaged over.                |
   +---------------------+------------------------------------------------------------------------+
   | VERSION             | Specifies which kernel version to run. All kernels are gradually       |
   |                     | optimized from a fundamental implementation, and all implementations   |
   |                     | along the way are kept for benchmarking purposes, i.e., to aquire      |
   |                     | insight about the speedups and memory usage gained from optimizations. |
   |                     |                                                                        |
   |                     | Currently, these are the implemented versions:                         |
   |                     |                                                                        |
   |                     | * ADD v1: Fundamental (see thesis report for description).             |
   |                     | * ADD v2: Implements a parameterized sequentialization factor to v1.   |
   |                     | * ADD v3: Implements parameterized number of instances per block to v2.|
   |                     |                                                                        |
   |                     | * MUL v1: Fundamental (see thesis report for description).             |
   |                     | * MUL v2: Implements optimized convulution computations to v1.         |
   |                     | * MUL v3: Implements parameterized number of instances per block to v2.|
   |                     | * MUL v4: Implements a fixed sequentialization factor of 4 to v2.      |
   |                     | * MUL v5: Implements parameterized number of instances per block to v4.|
   |                     |                                                                        |
   |                     | If VERSION is set to 0, then all kernel versions are run.              |
   +---------------------+------------------------------------------------------------------------+
   +----------------------------------------------------------------------------------------------+
   | Boolean parameters                                                                           |
   +=====================+========================================================================+
   | ADD                 | Runs addition kernel(s).                                               |
   +---------------------+------------------------------------------------------------------------+
   | MUL                 | Runs multiplication kernel(s).                                         |
   +---------------------+------------------------------------------------------------------------+
   | BASE32              | Run kernel(s) with big ints of base uint32_t.                          |
   +---------------------+------------------------------------------------------------------------+
   | BASE64              | Runs kernel(s) with big ints of base uint64_t.                         |
   +---------------------+------------------------------------------------------------------------+
   | ADD_TEN             | Runs the 10 addition kernel(s) instead of the 1 addition kernel, i.e., |
   |                     | kernel(s) computing `(9 * as) + bs` instead of `as + bs`.              |
   +---------------------+------------------------------------------------------------------------+
   | MUL_SIX             | Runs the 6 multiplication kernel(s) instead of the 1 multiplication    |
   |                     | i.e., kernel(s) computing `as^6 * bs` instead of `as * bs`.            |
   +---------------------+------------------------------------------------------------------------+
   | WITH_VALIDATION     | (In)validates the output of kernel(s).                                 |
   +---------------------+------------------------------------------------------------------------+
   | PRINT_DEBUG_INFO    | Prints additional information about kernel runs (useful for debugging).|
   +---------------------+------------------------------------------------------------------------+
   | FULL_TEST_SUITE     | By default, kernel(s) are run on 65536-bit big integers. If this       |
   |                     | parameter is set, it also runs on smaller big integers, specificically,|
   |                     | 32768-bit, 16384-bit, 8192-bit, 4096-bit, 2048-bit, 1024-bit & 512-bit.|
   +---------------------+------------------------------------------------------------------------+
   | FULLLER_TEST_SUITE  | By default, kernel(s) are run on 65536-bit big integers. If this       |
   |                     | parameter is set, it also runs on bigger big integers, specificically, |
   |                     | 131072-bit, 262144-bit & 524288-bit.                                   |
   +---------------------+------------------------------------------------------------------------+
*/

/*************************************************************************************************/
/*** Big Integer Addition ************************************************************************/
/*************************************************************************************************/

/* wrapper that invokes and times a big integer addition GPU kernels;
   m is the size in Base::uint_t units and v is the kernel version */
template<class Base, uint32_t m, uint32_t v>
void gpuAdd (uint32_t num_instances, typename Base::uint_t* h_as,
             typename Base::uint_t* h_bs, typename Base::uint_t* h_rs) {

    assert(Base::bits >= 32 && Base::bits % 32 == 0);
    assert(v >= 1 && v <= 3);

    using uint_t = typename Base::uint_t;
    uint_t* d_as;
    uint_t* d_bs;
    uint_t* d_rs;
    size_t mem_size_nums = num_instances * m * sizeof(uint_t);

    // 1. allocate device memory
    cudaMalloc((void**) &d_as, mem_size_nums);
    cudaMalloc((void**) &d_bs, mem_size_nums);
    cudaMalloc((void**) &d_rs, mem_size_nums);
 
    // 2. copy host memory to device
    cudaMemcpy(d_as, h_as, mem_size_nums, cudaMemcpyHostToDevice);
    cudaMemcpy(d_bs, h_bs, mem_size_nums, cudaMemcpyHostToDevice);

    // 3. kernel dimensions
    const uint32_t q = (v == 1) ? 1 : (m <= 1024) ? 4 : (m+1024-1) / 1024; // ceil(m/1024)
    assert(m%q == 0 && m >= q && m/q <= 1024);
    const uint32_t ipb = (v == 3) ? (128 + m/q - 1) / (m/q) : 1; // ceil(128/(m/q))
    dim3 block(ipb*(m/q), 1, 1);
    dim3 grid (num_instances/ipb, 1, 1);

    #if PRINT_DEBUG_INFO
    printf("[DEBUG] ipb: %d, num_instances: %d, q: %d, m: %d\n", ipb, num_instances, q, m);
    #endif

    // 4. run addition(s)
    {
        // 4.1 dry run
        #if !ADD_TEN
        if      (v == 1)
            baddKer1<Base,m>      <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 2)
            baddKer2<Base,m,q>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else // (v == 3)
            baddKer3<Base,m,q,ipb><<< grid, block >>>(d_as, d_bs, d_rs);
        #endif
        #if ADD_TEN
        if      (v == 1)
            baddKer1Bench<Base,m,10>      <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 2)
            baddKer2Bench<Base,m,q,10>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else // (v == 3)
            baddKer3Bench<Base,m,q,ipb,10><<< grid, block >>>(d_as, d_bs, d_rs);
        #endif

        cudaDeviceSynchronize();
        gpuAssert( cudaPeekAtLastError() );

        // 4.2 timing instrumentation and kernel invocation
        uint64_t elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        #if !ADD_TEN
        if      (v == 1)
            for(int i=0; i<GPU_RUNS_ADD; i++)
                baddKer1<Base,m>      <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 2)
            for(int i=0; i<GPU_RUNS_ADD; i++)
                baddKer2<Base,m,q>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else // (v == 3)
            for(int i=0; i<GPU_RUNS_ADD; i++)
                baddKer3<Base,m,q,ipb><<< grid, block >>>(d_as, d_bs, d_rs);
        #endif
        #if ADD_TEN
        if      (v == 1)
            for(int i=0; i<GPU_RUNS_ADD; i++)
                baddKer1Bench<Base,m,10>      <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 2)
            for(int i=0; i<GPU_RUNS_ADD; i++)
                baddKer2Bench<Base,m,q,10>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else // (v == 3)
            for(int i=0; i<GPU_RUNS_ADD; i++)
                baddKer3Bench<Base,m,q,ipb,10><<< grid, block >>>(d_as, d_bs, d_rs);
        #endif

        cudaDeviceSynchronize();

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / GPU_RUNS_ADD;

        // 4.3 print results
        gpuAssert( cudaPeekAtLastError() );

        double runtime_microsecs = elapsed;
        double bytes_accesses = 3.0 * num_instances * m * sizeof(uint_t);
        double gigabytes = bytes_accesses / (runtime_microsecs * 1000);

        #if !ADD_TEN
        printf("ONE V%d Addition of ", v);
        #endif
        #if ADD_TEN
        printf("TEN V%d Additions of ", v);
        #endif
        printf("%d-bit big integers (base u%d) runs %d instances in:\t%lu microsecs, GB/sec: %.2f\n"
               , m*Base::bits, Base::bits,num_instances, elapsed, gigabytes);

        cudaMemcpy(h_rs, d_rs, mem_size_nums, cudaMemcpyDeviceToHost);
        cudaDeviceSynchronize();
    }

    // 5. cleanup
    cudaFree(d_as);
    cudaFree(d_bs);
    cudaFree(d_rs);
}

/* performs the big integer addition(s) using gmp, obtaining a reference point for validation */
template<int m>
void gmpAdd(int num_instances, uint32_t* as, uint32_t* bs, uint32_t* rs) {
    uint32_t* it_as = as;
    uint32_t* it_bs = bs;
    uint32_t* it_rs = rs;

    for(int i=0; i<num_instances; i++) {
        gmpAddMulOnce<m>(true, it_as, it_bs, it_rs);
        it_as += m; it_bs += m; it_rs += m;
    }

    #if ADD_TEN
    for(int j=1; j<10; j++) {
        it_as = as; it_bs = rs; it_rs = rs;
        for(int i=0; i<num_instances; i++) {
            gmpAddMulOnce<m>(true, it_as, it_bs, it_rs);
            it_as += m; it_bs += m; it_rs += m;
        }
    }
    #endif
}

/* runs and possibly validates a big integer addition kernel */
template<class Base, uint32_t m, uint32_t v> // note, m is the size of the big word in u32 units
void testAddition(int num_instances, uint64_t* h_as_64, uint64_t* h_bs_64,
                  uint64_t* h_rs_gmp_64, uint64_t* h_rs_our_64) {
    
    assert(Base::bits >= 32 && Base::bits % 32 == 0);

    using uint_t = typename Base::uint_t;
    uint_t *h_as = (uint_t*) h_as_64;
    uint_t *h_bs = (uint_t*) h_bs_64;
    uint_t *h_rs_our = (uint_t*) h_rs_our_64;
    const uint32_t mb = m/(Base::bits/32);

    if (v == 1 && mb > 1024) {
        printf("SKIPS V1 Addition - big int size (%d precision of u%d) exceeds CUDA block \
limit (1024)\n", mb, Base::bits);
    }
    else {
        gpuAdd<Base,mb,v>(num_instances, h_as, h_bs, h_rs_our);
        #if WITH_VALIDATION
        uint32_t *h_rs_gmp_32 = (uint32_t*) h_rs_gmp_64;
        gmpAdd<m>(num_instances, (uint32_t*)h_as, (uint32_t*)h_bs, h_rs_gmp_32);
        validateExact(h_rs_gmp_32, (uint32_t*)h_rs_our, num_instances*m);
        #endif
    }
}

/*************************************************************************************************/
/*** Big Integer Multiplication ******************************************************************/
/*************************************************************************************************/

/* wrapper that invokes and times a big integer multiplication GPU kernels;
   m is the size in Base::uint_t units and v is the kernel version */
template<class Base, uint32_t m, uint32_t v>
void gpuMultiply(uint32_t num_instances, typename Base::uint_t* h_as,
                  typename Base::uint_t* h_bs, typename Base::uint_t* h_rs) {

    assert(Base::bits >= 32 && Base::bits % 32 == 0);
    assert(v >= 1 && v <= 5);

    using uint_t = typename Base::uint_t;
    uint_t* d_as;
    uint_t* d_bs;
    uint_t* d_rs;
    size_t mem_size_nums = num_instances * m * sizeof(uint_t);

    // 1. allocate device memory
    cudaMalloc((void**) &d_as, mem_size_nums);
    cudaMalloc((void**) &d_bs, mem_size_nums);
    cudaMalloc((void**) &d_rs, mem_size_nums);
 
    // 2. copy host memory to device
    cudaMemcpy(d_as, h_as, mem_size_nums, cudaMemcpyHostToDevice);
    cudaMemcpy(d_bs, h_bs, mem_size_nums, cudaMemcpyHostToDevice);

    // 3. kernel dimensions
    const uint32_t q = (v >= 1 && v <= 3) ? 2 : 4;
    assert(m%q == 0 && m >= q && m/q <= 1024);
    const uint32_t ipb = (v == 3 || v == 5) ? (256 + m - 1) / m : 1; // TODO finetune after v5 valid
    dim3 block(ipb*(m/q), 1, 1);
    dim3 grid (num_instances/ipb, 1, 1);
    #if PRINT_DEBUG_INFO
    printf("[DEBUG] ipb: %d, num_instances: %d, q: %d, m: %d\n", ipb, num_instances, q, m);
    #endif

    // 4. one multiplication
    {
        // 4.1 dry run
        #if !MUL_SIX
        if      (v == 1)
            convMult1<Base,m>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 2)
            convMult2<Base,m>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 3)
            convMult3<Base,m,ipb><<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 4)
            convMult4<Base,m>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else // (v == 5)
            convMult5<Base,m,ipb><<< grid, block >>>(d_as, d_bs, d_rs);
        #endif
        #if MUL_SIX
        if      (v == 1)
            convMult1Bench<Base,m,6>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 2)
            convMult2Bench<Base,m,6>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 3)
            convMult3Bench<Base,m,ipb,6><<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 4)
            convMult4Bench<Base,m,6>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else // (v == 5)
            convMult5Bench<Base,m,ipb,6><<< grid, block >>>(d_as, d_bs, d_rs);
        #endif
        
        cudaDeviceSynchronize();
        gpuAssert( cudaPeekAtLastError() );

        // 4.2 timing instrumentation and kernel invocation
        uint64_t elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        #if !MUL_SIX
        if      (v == 1)
            for(int i=0; i<GPU_RUNS_MUL; i++)
                convMult1<Base,m>     <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 2)
            for(int i=0; i<GPU_RUNS_MUL; i++)
                convMult2<Base,m>     <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 3)
             for(int i=0; i<GPU_RUNS_MUL; i++)
                 convMult3<Base,m,ipb><<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 4)
            for(int i=0; i<GPU_RUNS_MUL; i++)
                convMult4<Base,m>     <<< grid, block >>>(d_as, d_bs, d_rs);
        else // (v == 5)
             for(int i=0; i<GPU_RUNS_MUL; i++)
                 convMult5<Base,m,ipb><<< grid, block >>>(d_as, d_bs, d_rs);
        #endif
        #if MUL_SIX
        if      (v == 1)
            for(int i=0; i<GPU_RUNS_MUL; i++)
                convMult1Bench<Base,m,6>     <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 2)
            for(int i=0; i<GPU_RUNS_MUL; i++)
                convMult2Bench<Base,m,6>     <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 3)
             for(int i=0; i<GPU_RUNS_MUL; i++)
                 convMult3Bench<Base,m,ipb,6><<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 4)
            for(int i=0; i<GPU_RUNS_MUL; i++)
                convMult4Bench<Base,m,6>     <<< grid, block >>>(d_as, d_bs, d_rs);
        else // (v == 5)
             for(int i=0; i<GPU_RUNS_MUL; i++)
                 convMult5Bench<Base,m,ipb,6><<< grid, block >>>(d_as, d_bs, d_rs);
        #endif

        cudaDeviceSynchronize();

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / GPU_RUNS_ADD;

        // 4.3 print results
        gpuAssert( cudaPeekAtLastError() );

        double runtime_microsecs = elapsed;
        double num_u32_ops = 4.0 * num_instances * (m * Base::bits / 32) * (m * Base::bits / 32);
        #if MUL_SIX
        num_u32_ops *= 6;
        #endif
        double gigaopsu32 = num_u32_ops / (runtime_microsecs * 1000);

        #if !MUL_SIX
        printf("ONE V%d Multiplication of ", v);
        #endif
        #if MUL_SIX
        printf("SIX V%d Multiplications of ", v);
        #endif
        printf("%d-bit big integers (base u%d) runs %d instances in:\t%lu microsecs, \
Gu32ops/sec: %.2f\n", m*Base::bits, Base::bits, num_instances, elapsed, gigaopsu32);

        cudaMemcpy(h_rs, d_rs, mem_size_nums, cudaMemcpyDeviceToHost);
        cudaDeviceSynchronize();
    }

    // 5. cleanup
    cudaFree(d_as);
    cudaFree(d_bs);
    cudaFree(d_rs);
}

/* performs the big integer multiplication(s) using gmp, obtaining a reference point for validation*/
template<int m>
void gmpMultiply(int num_instances, uint32_t* as, uint32_t* bs, uint32_t* rs) {
    uint32_t* it_as = as;
    uint32_t* it_bs = bs;
    uint32_t* it_rs = rs;

    for(int i=0; i<num_instances; i++) {
        gmpAddMulOnce<m>(false, it_as, it_bs, it_rs);
        it_as += m; it_bs += m; it_rs += m;
    }

    #if MUL_SIX
    for(int j=1; j<6; j++) {
        it_as = as; it_bs = rs; it_rs = rs;
        for(int i=0; i<num_instances; i++) {
            gmpAddMulOnce<m>(false, it_as, it_bs, it_rs);
            it_as += m; it_bs += m; it_rs += m;
        }
    }
    #endif
}

/* runs and possibly validates a big integer multiplication kernel */
template<typename Base, uint32_t m, uint32_t v>  // note, m is the size of the big word in u32 units
void testMul(int num_instances, uint64_t* h_as_64, uint64_t* h_bs_64,
             uint64_t* h_rs_gmp_64, uint64_t* h_rs_our_64) {

    assert(Base::bits >= 32 && Base::bits % 32 == 0);

    using uint_t = typename Base::uint_t;
    uint_t *h_as = (uint_t*)h_as_64;
    uint_t *h_bs = (uint_t*)h_bs_64;
    uint_t *h_rs_our = (uint_t*)h_rs_our_64;
    const uint32_t mb = m/(Base::bits/32);

    if ((v == 1 && mb/2 > 1024) || ((v == 2 || v == 3 || v == 4 || v == 5) && mb/4 > 1024)) {
        printf("SKIPS V%d Multiplication - big int size (%d precision of base u%d) exceeds CUDA \
block limit (1024)\n", v, mb, Base::bits);
    }
    else {
        gpuMultiply<Base,mb,v>(num_instances, h_as, h_bs, h_rs_our);
        #if WITH_VALIDATION
        uint32_t *h_rs_gmp_32 = (uint32_t*)h_rs_gmp_64;
        gmpMultiply<m>(num_instances, (uint32_t*)h_as, (uint32_t*)h_bs, h_rs_gmp_32);
        validateExact(h_rs_gmp_32, (uint32_t*)h_rs_our, num_instances*m);
        #endif
    }
}

/*************************************************************************************************/
/*** Main program ********************************************************************************/
/*************************************************************************************************/

/* runs a big integer addition kernel according to main */
template<typename Base, uint32_t v>
void runAdditions(uint64_t total_work, uint64_t* h_as, uint64_t* h_bs,
                  uint64_t* h_rs_gmp, uint64_t* h_rs_our) {
    #if FULLER_TEST_SUITE
    testAddition<Base,16384,v>( total_work/16384, h_as, h_bs, h_rs_gmp, h_rs_our );
    testAddition<Base,8192,v> ( total_work/8192,  h_as, h_bs, h_rs_gmp, h_rs_our );
    testAddition<Base,4096,v> ( total_work/4096,  h_as, h_bs, h_rs_gmp, h_rs_our );
    #endif
    testAddition<Base,2048,v> ( total_work/2048,  h_as, h_bs, h_rs_gmp, h_rs_our );
    #if FULL_TEST_SUITE
    testAddition<Base,1024,v> ( total_work/1024,  h_as, h_bs, h_rs_gmp, h_rs_our );
    testAddition<Base,512,v>  ( total_work/512,   h_as, h_bs, h_rs_gmp, h_rs_our );
    testAddition<Base,256,v>  ( total_work/256,   h_as, h_bs, h_rs_gmp, h_rs_our );
    testAddition<Base,128,v>  ( total_work/128,   h_as, h_bs, h_rs_gmp, h_rs_our );
    testAddition<Base,64,v>   ( total_work/64,    h_as, h_bs, h_rs_gmp, h_rs_our );
    testAddition<Base,32,v>   ( total_work/32,    h_as, h_bs, h_rs_gmp, h_rs_our );
    testAddition<Base,16,v>   ( total_work/16,    h_as, h_bs, h_rs_gmp, h_rs_our );
    #endif
    printf("\n");
}

/* runs a big integer multiplication kernel according to main */
template<typename Base, uint32_t v>
void runMultiplications(uint64_t total_work, uint64_t* h_as, uint64_t* h_bs,
                        uint64_t* h_rs_gmp, uint64_t* h_rs_our) {
    #if FULLER_TEST_SUITE
    testMul<Base,16384,v>( total_work/16384, h_as, h_bs, h_rs_gmp, h_rs_our );
    testMul<Base,8192,v> ( total_work/8192,  h_as, h_bs, h_rs_gmp, h_rs_our );
    testMul<Base,4096,v> ( total_work/4096,  h_as, h_bs, h_rs_gmp, h_rs_our );
    #endif
    testMul<Base,2048,v> ( total_work/2048,  h_as, h_bs, h_rs_gmp, h_rs_our );
    #if FULL_TEST_SUITE
    testMul<Base,1024,v> ( total_work/1024,  h_as, h_bs, h_rs_gmp, h_rs_our );
    testMul<Base,512,v>  ( total_work/512,   h_as, h_bs, h_rs_gmp, h_rs_our );
    testMul<Base,256,v>  ( total_work/256,   h_as, h_bs, h_rs_gmp, h_rs_our );
    testMul<Base,128,v>  ( total_work/128,   h_as, h_bs, h_rs_gmp, h_rs_our );
    testMul<Base,64,v>   ( total_work/64,    h_as, h_bs, h_rs_gmp, h_rs_our );
    testMul<Base,32,v>   ( total_work/32,    h_as, h_bs, h_rs_gmp, h_rs_our );
    testMul<Base,16,v>   ( total_work/16,    h_as, h_bs, h_rs_gmp, h_rs_our );
    #endif
    printf("\n");
}

/* see top of file for a description of main */
int main(int argc, char * argv[]) {
    if (argc != 2) {
        printf("Usage: %s <batch-size>\n", argv[0]);
        exit(1);
    }   printf("\n");

    const int total_work = atoi(argv[1]);
    uint64_t *h_as, *h_bs, *h_rs_gmp, *h_rs_our;
    mkRandArrays<32,32>( total_work/32, &h_as, &h_bs, &h_rs_gmp, &h_rs_our );

#if ADD
# if BASE32
#  if VERSION == 0 || VERSION == 1
    runAdditions<U32bits,1>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
#  if VERSION == 0 || VERSION == 2
    runAdditions<U32bits,2>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
#  if VERSION == 0 || VERSION == 3
    runAdditions<U32bits,3>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
# endif
# if BASE64
#  if VERSION == 0 || VERSION == 1
    runAdditions<U64bits,1>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
#  if VERSION == 0 || VERSION == 2
    runAdditions<U64bits,2>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
#  if VERSION == 0 || VERSION == 3
    runAdditions<U64bits,3>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
# endif
#endif

#if MUL
# if BASE32
#  if VERSION == 0 || VERSION == 1
    runMultiplications<U32bits,1>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
#  if VERSION == 0 || VERSION == 2
    runMultiplications<U32bits,2>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
#  if VERSION == 0 || VERSION == 3
    runMultiplications<U32bits,3>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
#  if VERSION == 0 || VERSION == 4
    runMultiplications<U32bits,4>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
#  if VERSION == 0 || VERSION == 5
    runMultiplications<U32bits,5>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
# endif
# if BASE64
#  if VERSION == 0 || VERSION == 1
    runMultiplications<U64bits,1>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
#  if VERSION == 0 || VERSION == 2
    runMultiplications<U64bits,2>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
#  if VERSION == 0 || VERSION == 3
    runMultiplications<U64bits,3>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
#  if VERSION == 0 || VERSION == 4
    runMultiplications<U64bits,4>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
#  if VERSION == 0 || VERSION == 5
    runMultiplications<U64bits,5>(total_work, h_as, h_bs, h_rs_gmp, h_rs_our);
#  endif
# endif
#endif

    free(h_as);
    free(h_bs);
    free(h_rs_gmp);
    free(h_rs_our);
}
