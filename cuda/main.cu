#include "helpers.h"
#include "kernels.cu.h"

using namespace std;

#define GPU_RUNS_ADD    300
#define WITH_VALIDATION 1
#define DEBUG           0

/****************************/
/*** Big-Integer Addition ***/
/****************************/

// wrapper that invokes and times the big int addition GPU kernels;
// m is the size in Base::uint_t units and v is the kernel version.
template<class Base, uint32_t m, uint32_t v>
void gpuAdd (uint32_t num_instances, typename Base::uint_t* h_as,
             typename Base::uint_t* h_bs, typename Base::uint_t* h_rs) {

    assert((Base::bits >= 32) && (Base::bits % 32 == 0));

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
    const uint32_t q = (v > 1) ? 4 : 1;
    assert(m%q == 0 && m >= q && m/q <= 1024);
    const uint32_t ipb = (v > 2) ? (128 + m/q - 1) / (m/q) : 1; // ceil(128/(m/q))
    dim3 block(ipb*(m/q), 1, 1);
    dim3 grid (num_instances/ipb, 1, 1);
    #if DEBUG
    printf("\n[debug] ipb: %d, num_instances: %d, q: %d, m: %d\n", ipb, num_instances, q, m);
    #endif

    // 4. one addition
    {
        // dry run
        if (v == 1)
            baddKer1<Base,m>      <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 2)
            baddKer2<Base,m,q>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else // v == 3
            baddKer3<Base,m,q,ipb><<< grid, block >>>(d_as, d_bs, d_rs);
    
        cudaDeviceSynchronize();
        gpuAssert( cudaPeekAtLastError() );

        // timing instrumentation
        uint64_t elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        if (v == 1)
            for(int i=0; i<GPU_RUNS_ADD; i++)
                baddKer1<Base,m>      <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 2)
            for(int i=0; i<GPU_RUNS_ADD; i++)
                baddKer2<Base,m,q>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else // v == 3
            for(int i=0; i<GPU_RUNS_ADD; i++)
                baddKer3<Base,m,q,ipb><<< grid, block >>>(d_as, d_bs, d_rs);

        cudaDeviceSynchronize();

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / GPU_RUNS_ADD;

        // print results
        gpuAssert( cudaPeekAtLastError() );

        double runtime_microsecs = elapsed; 
        double bytes_accesses = 3.0 * num_instances * m * sizeof(uint_t);  
        double gigabytes = bytes_accesses / (runtime_microsecs * 1000);

        printf("One v%d addition  of %d-bit big integers (base u%d) runs %d instances \
in:\t%lu microsecs, GB/sec: %.2f, Mil-Instances/sec: %.2f\n", v, m*Base::bits, Base::bits,
               num_instances, elapsed, gigabytes, num_instances/runtime_microsecs);
        
        cudaMemcpy(h_rs, d_rs, mem_size_nums, cudaMemcpyDeviceToHost);
        cudaDeviceSynchronize();
    }

    // 5. ten additions
    {
        // dry run
        if      (v == 1)
            baddKer1Bench<Base,m,10>      <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 2)
            baddKer2Bench<Base,m,q,10>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else // (v == 3)
            baddKer3Bench<Base,m,q,ipb,10><<< grid, block >>>(d_as, d_bs, d_rs);
    
        cudaDeviceSynchronize();
        gpuAssert( cudaPeekAtLastError() );

        // timing instrumentation
        uint64_t elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        if      (v == 1)
            for(int i=0; i<GPU_RUNS_ADD; i++)
                baddKer1Bench<Base,m,10>      <<< grid, block >>>(d_as, d_bs, d_rs);
        else if (v == 2)
            for(int i=0; i<GPU_RUNS_ADD; i++)
                baddKer2Bench<Base,m,q,10>    <<< grid, block >>>(d_as, d_bs, d_rs);
        else // (v == 3)
            for(int i=0; i<GPU_RUNS_ADD; i++)
                baddKer3Bench<Base,m,q,ipb,10><<< grid, block >>>(d_as, d_bs, d_rs);

        cudaDeviceSynchronize();

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / GPU_RUNS_ADD;

        // print results
        gpuAssert( cudaPeekAtLastError() );

        double runtime_microsecs = elapsed; 
        double bytes_accesses = 3.0 * num_instances * m * sizeof(uint_t);  
        double gigabytes = bytes_accesses / (runtime_microsecs * 1000);

        printf("Ten v%d additions of %d-bit big integers (base u%d) runs %d instances \
in:\t%lu microsecs, GB/sec: %.2f, Mil-Instances/sec: %.2f\n", v, m*Base::bits, Base::bits,
               num_instances, elapsed, gigabytes, num_instances/runtime_microsecs);
    }

    // 6. cleanup
    cudaFree(d_as);
    cudaFree(d_bs);
    cudaFree(d_rs);
}

// performs big int addition using gmp
template<int m>
void gmpAdd(int num_instances, uint32_t* as, uint32_t* bs, uint32_t* rs) {
    uint32_t* it_as = as;
    uint32_t* it_bs = bs;
    uint32_t* it_rs = rs;
        
    for(int i=0; i<num_instances; i++) {
        gmpAddMulOnce<m>(true, it_as, it_bs, it_rs);
        it_as += m; it_bs += m; it_rs += m;
    }
}

// wrapper that benchmarks (and possible validates) the big int addition kernels;
// m is the size of the big word in u32 units.
template<class Base, int m>
void testAddition(int num_instances, uint64_t* h_as_64, uint64_t* h_bs_64,
                  uint64_t* h_rs_gmp_64, uint64_t* h_rs_our_64, uint32_t with_validation) {
    
    assert((Base::bits >= 32) && (Base::bits % 32 == 0));
    printf("\n");

    using uint_t = typename Base::uint_t;
    uint_t *h_as = (uint_t*) h_as_64;
    uint_t *h_bs = (uint_t*) h_bs_64;
    uint_t *h_rs_our = (uint_t*) h_rs_our_64;
    uint32_t *h_rs_gmp_32 = (uint32_t*) h_rs_gmp_64;

    if(with_validation)
        gmpAdd<m>(num_instances, (uint32_t*)h_as, (uint32_t*)h_bs, h_rs_gmp_32);

    gpuAdd<Base,m/(Base::bits/32),1>(num_instances, h_as, h_bs, h_rs_our);
    if(with_validation)
        validateExact(h_rs_gmp_32, (uint32_t*)h_rs_our, num_instances*m);

    gpuAdd<Base,m/(Base::bits/32),2>(num_instances, h_as, h_bs, h_rs_our);
    if(with_validation)
        validateExact(h_rs_gmp_32, (uint32_t*)h_rs_our, num_instances*m);

    gpuAdd<Base,m/(Base::bits/32),3>(num_instances, h_as, h_bs, h_rs_our);
    if(with_validation)
        validateExact(h_rs_gmp_32, (uint32_t*)h_rs_our, num_instances*m);
}

/*****************************************/
/*** Main program that runs test suits ***/
/*****************************************/

// runs the big int addition kernel benchmarks and tests.
template<typename Base>
void runAdditions(uint64_t total_work) {
    uint64_t *h_as, *h_bs, *h_rs_gmp, *h_rs_our;
    mkRandArrays<32,32>( total_work/32, &h_as, &h_bs, &h_rs_gmp, &h_rs_our );

    testAddition<Base, 2048>( total_work/2048, h_as, h_bs, h_rs_gmp, h_rs_our, WITH_VALIDATION );
    testAddition<Base, 1024>( total_work/1024, h_as, h_bs, h_rs_gmp, h_rs_our, WITH_VALIDATION );
    testAddition<Base, 512> ( total_work/512,  h_as, h_bs, h_rs_gmp, h_rs_our, WITH_VALIDATION );
    testAddition<Base, 256> ( total_work/256,  h_as, h_bs, h_rs_gmp, h_rs_our, WITH_VALIDATION );
    testAddition<Base, 128> ( total_work/128,  h_as, h_bs, h_rs_gmp, h_rs_our, WITH_VALIDATION );
    testAddition<Base, 64>  ( total_work/64,   h_as, h_bs, h_rs_gmp, h_rs_our, WITH_VALIDATION );
    testAddition<Base, 32>  ( total_work/32,   h_as, h_bs, h_rs_gmp, h_rs_our, WITH_VALIDATION );
    testAddition<Base, 16>  ( total_work/16,   h_as, h_bs, h_rs_gmp, h_rs_our, WITH_VALIDATION );

    free(h_as);
    free(h_bs);
    free(h_rs_gmp);
    free(h_rs_our);
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        printf("Usage: %s <batch-size>\n", argv[0]);
        exit(1);
    }
    const int total_work = atoi(argv[1]);

    runAdditions<U64bits>(total_work);
}
