#include "../helpers.h"
#include <cuda.h>
#include "include/cgbn/cgbn.h"

/* This file is heavily based on:

   ``GPU Implementations for Midsize Integer Addition and Multiplication'',
   by Cosmin E. Oancea and Stephen M. Watt, 2024. [2]
   paper published:       https://arxiv.org/abs/2405.14642
   source code published: https://github.com/coancea/midint-arithmetic
*/

// IMPORTANT:  DO NOT DEFINE TPI OR BITS BEFORE INCLUDING CGBN
#define TPI  THD_PER_INST // at least 8 words per thread 
#define BITS (NUM_BITS)//2048 //3200

#include "cgbn-kers.cu.h"

#define GPU_RUNS_ADD  300
#define GPU_RUNS_CMUL 100
#define ADD 1
#define MUL 1

/****************************/
/***  support routines    ***/
/****************************/

void cgbn_check(cgbn_error_report_t *report, const char *file=NULL, int32_t line=0) {
    // check for cgbn errors
    if(cgbn_error_report_check(report)) {
        printf("\n");
        printf("CGBN error occurred: %s\n", cgbn_error_string(report));

        if(report->_instance!=0xFFFFFFFF) {
            printf("Error reported by instance %d", report->_instance);
            if(report->_blockIdx.x!=0xFFFFFFFF || report->_threadIdx.x!=0xFFFFFFFF)
                printf(", ");
            if(report->_blockIdx.x!=0xFFFFFFFF)
                printf("blockIdx=(%d, %d, %d) ",
                       report->_blockIdx.x, report->_blockIdx.y, report->_blockIdx.z);
            if(report->_threadIdx.x!=0xFFFFFFFF)
                printf("threadIdx=(%d, %d, %d)",
                       report->_threadIdx.x, report->_threadIdx.y, report->_threadIdx.z);
            printf("\n");
        }
        else {
            printf("Error reported by blockIdx=(%d %d %d)",
                   report->_blockIdx.x, report->_blockIdx.y, report->_blockIdx.z);
            printf("threadIdx=(%d %d %d)\n",
                   report->_threadIdx.x, report->_threadIdx.y, report->_threadIdx.z);
        }
        if(file!=NULL)
            printf("file %s, line %d\n", file, line);
        exit(1);
    }
}
#define CGBN_CHECK(report) cgbn_check(report, __FILE__, __LINE__)

// support routine to generate random instances
instance_t *generate_instances(uint32_t count) {
    instance_t *instances=(instance_t *)malloc(sizeof(instance_t)*count);

    for(int index=0;index<count;index++) {
        ourMkRandom<BITS/32, BITS/32>(1, instances[index].a._limbs);
        ourMkRandom<BITS/32, BITS/32>(1, instances[index].b._limbs);
    }
    return instances;
}

void verifyResults(bool is_add, uint32_t num_instances, instance_t  *instances) {
    uint32_t buffer[BITS/32];
    for(uint32_t i=0; i<num_instances; i++) {
        gmpAddMulOnce<BITS/32>(is_add, &instances[i].a._limbs[0], &instances[i].b._limbs[0],
                               &buffer[0]);
        for(uint32_t j=0; j<BITS/32; j++) {
            if ( buffer[j] != instances[i].sum._limbs[j] ) {
                printf( "INVALID RESULT at instance: %u, local index %u: %u vs %u\n",
                        i, j, buffer[j], instances[i].sum._limbs[j]);
                return;
            }
        }
    }
    printf("VALID!\n");
}

void runAdd ( const uint32_t num_instances, const uint32_t cuda_block, cgbn_error_report_t *report,
              instance_t  *gpuInstances, instance_t  *instances) {

    const uint32_t ipb = cuda_block/TPI;

    // start timer
    unsigned long int elapsed = 0;
    struct timeval t_start, t_end, t_diff;
    gettimeofday(&t_start, NULL);

    // launch with 32 threads per instance, 128 threads (4 instances) per block
    for(int i = 0; i < GPU_RUNS_ADD; i++)
        kernel_add<<<(num_instances+ipb-1)/ipb, cuda_block>>>(report, gpuInstances, num_instances);
    cudaDeviceSynchronize();

    //end timer
    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / GPU_RUNS_ADD;

    gpuAssert( cudaPeekAtLastError() );

    const uint32_t m = BITS / 32;
    double runtime_microsecs = elapsed;
    double bytes_accesses = 3.0 * num_instances * m * sizeof(uint32_t);
    double gigabytes = bytes_accesses / (runtime_microsecs * 1000);

    printf( "CGBN Addition (num-instances = %d, num-word-len = %d, total-size: %d) \
runs in: %lu microsecs, GB/sec: %.2f\n",
            num_instances, m, num_instances * m, elapsed, gigabytes);

    // error report uses managed memory, so we sync the device (or stream) and check for cgbn errors
    CUDA_CHECK(cudaDeviceSynchronize());
    CGBN_CHECK(report);

    // copy the instances back from gpuMemory
    CUDA_CHECK(cudaMemcpy(instances, gpuInstances, sizeof(instance_t)*num_instances,
                          cudaMemcpyDeviceToHost));

    verifyResults(true, num_instances, instances);

    { // testing 10 additions // kernel_10adds
        unsigned long int elapsed = 0;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL);

        // launch with 32 threads per instance, 128 threads (4 instances) per block
        for(int i = 0; i < GPU_RUNS_ADD; i++)
            kernel_10adds<<<(num_instances+ipb-1)/ipb, cuda_block>>>
                (report, gpuInstances, num_instances);
        cudaDeviceSynchronize();

        //end timer
        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / GPU_RUNS_ADD;

        gpuAssert( cudaPeekAtLastError() );

        const uint32_t m = BITS / 32;
        double runtime_microsecs = elapsed;
        double bytes_accesses = 3.0 * num_instances * m * sizeof(uint32_t);
        double gigabytes = bytes_accesses / (runtime_microsecs * 1000);

        printf( "CGBN TEN Additions (num-instances = %d, num-word-len = %d, total-size: %d) \
runs in: %lu microsecs, GB/sec: %.2f\n",
                num_instances, m, num_instances * m, elapsed, gigabytes);

        // error report uses managed memory, so we sync the device (or stream) and check for errors
        CUDA_CHECK(cudaDeviceSynchronize());
        CGBN_CHECK(report);
    }
}

void runMul ( const uint32_t num_instances, const uint32_t cuda_block, cgbn_error_report_t *report,
              instance_t  *gpuInstances, instance_t  *instances) {

    const uint32_t ipb = cuda_block/TPI;

    // start timer
    unsigned long int elapsed = 0;
    struct timeval t_start, t_end, t_diff;
    gettimeofday(&t_start, NULL);

    // launch with 32 threads per instance, 128 threads (4 instances) per block
    for(int i = 0; i < GPU_RUNS_CMUL; i++)
        kernel_mul<<<(num_instances+ipb-1)/ipb, cuda_block>>>(report, gpuInstances, num_instances);
    cudaDeviceSynchronize();

    //end timer
    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / GPU_RUNS_CMUL;

    gpuAssert( cudaPeekAtLastError() );

    const uint32_t m = BITS / 32;
    double runtime_microsecs = elapsed;
    uint64_t logM = 0, shiftM = m;
    while (shiftM >>= 1) logM++;
    double num_u32_ops = num_instances * 300 * m * logM;
    double gigaopsu32 = num_u32_ops / (runtime_microsecs * 1000);

    printf( "CGBN Multiply (num-instances = %d, num-word-len = %d, total-size: %d), \
averaged over %d runs: %lu microsecs, Gu32ops/sec: %.2f\n",
            num_instances, m, num_instances * m, GPU_RUNS_CMUL, elapsed, gigaopsu32);

    // error report uses managed memory, so we sync the device (or stream) and check for cgbn errors
    CUDA_CHECK(cudaDeviceSynchronize());
    CGBN_CHECK(report);

    // copy the instances back from gpuMemory
    CUDA_CHECK(cudaMemcpy(instances, gpuInstances, sizeof(instance_t)*num_instances,
                          cudaMemcpyDeviceToHost));

    verifyResults(false, num_instances, instances);

    { // testing 6 multiplications
        // start timer
        unsigned long int elapsed = 0;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL);

        // launch with 32 threads per instance, 128 threads (4 instances) per block
        for(int i = 0; i < GPU_RUNS_CMUL; i++)
            kernel_6mul<<<(num_instances+ipb-1)/ipb, cuda_block>>>
                (report, gpuInstances, num_instances);
        cudaDeviceSynchronize();

        //end timer
        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / GPU_RUNS_CMUL;

        gpuAssert( cudaPeekAtLastError() );

        const uint32_t m = BITS / 32;
        double runtime_microsecs = elapsed;
        uint64_t logM = 0, shiftM = m;
        while (shiftM >>= 1) logM++;
        double num_u32_ops = 6.0 * num_instances * 300 * m * logM;
        double gigaopsu32 = num_u32_ops / (runtime_microsecs * 1000);

        printf( "CGBN SIX Multiply (num-instances = %d, num-word-len = %d, total-size: %d), \
averaged over %d runs: %lu microsecs, Gu32ops/sec: %.2f\n",
                num_instances, m, num_instances * m, GPU_RUNS_CMUL, elapsed, gigaopsu32);

        // error report uses managed memory, so we sync the device (or stream) and check for errors
        CUDA_CHECK(cudaDeviceSynchronize());
        CGBN_CHECK(report);
    }
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        printf("Usage: %s <number-of-instances>\n", argv[0]);
        exit(1);
    }

    const int num_instances = atoi(argv[1]);

    instance_t          *instances, *gpuInstances;
    cgbn_error_report_t *report;

    instances=generate_instances(num_instances);

    CUDA_CHECK(cudaSetDevice(0));
    CUDA_CHECK(cudaMalloc((void **)&gpuInstances, sizeof(instance_t)*num_instances));
    CUDA_CHECK(cudaMemcpy(gpuInstances, instances, sizeof(instance_t)*num_instances,
                          cudaMemcpyHostToDevice));

    // create a cgbn_error_report for CGBN to report back errors
    CUDA_CHECK(cgbn_error_report_alloc(&report)); 

#if ADD
    runAdd (num_instances, 128, report, gpuInstances, instances);
#endif
#if MUL
    runMul (num_instances, 128, report, gpuInstances, instances);
#endif

    // clean up
    free(instances);
    CUDA_CHECK(cudaFree(gpuInstances));
    CUDA_CHECK(cgbn_error_report_free(report));

    return 0;
}
