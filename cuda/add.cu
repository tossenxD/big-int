#include "helper.h"
#include <cuda.h>
#include "add-kers.cu.h"

#define GPU_RUNS 10

template<class T>
void print_bigint(bigint_t<T>& bn) {
    printf("len: %d\nnum: [", bn.len);
    for (int i=0; i<bn.len; i++) {
	printf("%d", bn.num[i]);
	if (i != bn.len-1) { printf(", "); }
    }
    printf("]\ninfo: [");
    for (int i=0; i<bn.len; i++) {
	printf("%d", bn.info[i]);
	if (i != bn.len-1) { printf(", "); }
    }
    printf("]\n");
}

int main(int argc, char * argv[]) {
    // check input
    if (argc != 3) {
	printf("Usage: %s <number-of-instances> <size-of-bigints>\n", argv[0]);
	exit(1);
    }
    
    // setup constants
    const int num_instances = atoi(argv[1]);
    const int num_size      = atoi(argv[2]);
    const int block_size    = 512;
    const int num_blocks    = ((num_instances + (block_size - 1)) / block_size);
    const int mem_size      = sizeof(bigint_t<uint32_t>)*num_instances;
    bigint_t<uint32_t>* h_bigints_in1;
    bigint_t<uint32_t>* h_bigints_in2;
    bigint_t<uint32_t>* h_bigints_out;
    bigint_t<uint32_t>* d_bigints_in1;
    bigint_t<uint32_t>* d_bigints_in2;
    bigint_t<uint32_t>* d_bigints_out;

    // allocate and initialize host memory
    h_bigints_in1 = (bigint_t<uint32_t>*) malloc(mem_size);
    h_bigints_in2 = (bigint_t<uint32_t>*) malloc(mem_size);
    h_bigints_out = (bigint_t<uint32_t>*) malloc(mem_size);
    for (int i=0; i<num_instances; i++) {
	bigint_init<uint32_t> (h_bigints_in1[i], (uint8_t)num_size, (uint32_t)-1);
	bigint_init<uint32_t> (h_bigints_in2[i], (uint8_t)num_size, 1);
    }
    h_bigints_in1[0].num[1] = (uint32_t)-1;

    // allocate and initialize device memory
    CUDA_CHECK(cudaSetDevice(0));
    CUDA_CHECK(cudaMalloc((void**) &d_bigints_in1, mem_size));
    CUDA_CHECK(cudaMalloc((void**) &d_bigints_in2, mem_size));
    CUDA_CHECK(cudaMalloc((void**) &d_bigints_out, mem_size));
    CUDA_CHECK(cudaMemcpy(d_bigints_in1, h_bigints_in1, mem_size, cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_bigints_in2, h_bigints_in2, mem_size, cudaMemcpyHostToDevice));
    bigint_t<uint32_t>* tmp = (bigint_t<uint32_t>*) malloc(mem_size);
    for (int i=0; i<num_instances; i++) { bigint_init<uint32_t> (tmp[i], (uint8_t)num_size); }
    CUDA_CHECK(cudaMemcpy(d_bigints_out, tmp, mem_size, cudaMemcpyHostToDevice));
    free(tmp);

    // start timer
    unsigned long int elapsed = 0;
    struct timeval t_start, t_end, t_diff;
    gettimeofday(&t_start, NULL);

    // run kernel
    for (int i=0; i<GPU_RUNS; i++) {
	kernel_add<uint32_t><<<num_blocks, block_size>>>(d_bigints_in1, d_bigints_in2,
							 d_bigints_out, num_instances);
    }
    cudaDeviceSynchronize();

    //end timer
    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / GPU_RUNS;

    // results
    CUDA_CHECK(cudaMemcpy(h_bigints_out, d_bigints_out, mem_size, cudaMemcpyDeviceToHost));
    for (int i=0; i<num_instances; i++) {
	print_bigint<uint32_t> (h_bigints_out[i]);
    }

    // cleanup
    for (int i=0; i<num_instances; i++) {
	free(h_bigints_in1[i].num);
	free(h_bigints_in2[i].num);
    }
    free(h_bigints_in1);
    free(h_bigints_in2);
    free(h_bigints_out);
    CUDA_CHECK(cudaFree(d_bigints_in1));
    CUDA_CHECK(cudaFree(d_bigints_in2));
    CUDA_CHECK(cudaFree(d_bigints_out));
}
