#ifndef CGBN_KERNELS
#define CGBN_KERNELS

/* This file is heavily based on:

   ``GPU Implementations for Midsize Integer Addition and Multiplication'',
   by Cosmin E. Oancea and Stephen M. Watt, 2024. [2]
   paper published:       https://arxiv.org/abs/2405.14642
   source code published: https://github.com/coancea/midint-arithmetic
*/

// Declare the instance type
typedef struct {
  cgbn_mem_t<BITS> a;
  cgbn_mem_t<BITS> b;
  cgbn_mem_t<BITS> sum;
} instance_t;

// helpful typedefs for the kernel
typedef cgbn_context_t<TPI>         context_t;
typedef cgbn_env_t<context_t, BITS> env_t;

/***********************/
/*** Addition Kernel ***/
/***********************/

__global__ void kernel_add(cgbn_error_report_t *report, instance_t *instances, uint32_t count) {
  int32_t instance;

  // decode an instance number from the blockIdx and threadIdx
  instance=(blockIdx.x*blockDim.x + threadIdx.x)/TPI;
  if(instance>=count)
    return;

  // construct a context
  context_t      bn_context(cgbn_no_checks, NULL, instance);
  env_t          bn_env(bn_context.env<env_t>());      // construct an environment for 1024-bit math
  env_t::cgbn_t  a, b, r;                              // define a, b, r as 1024-bit bignums

  cgbn_load(bn_env, a, &(instances[instance].a));      // load my instance's a value
  cgbn_load(bn_env, b, &(instances[instance].b));      // load my instance's b value
  cgbn_add(bn_env, r, a, b);                           // r=a+b
  cgbn_store(bn_env, &(instances[instance].sum), r);   // store r into sum
}

__global__ void kernel_10adds(cgbn_error_report_t *report, instance_t *instances, uint32_t count) {
  int32_t instance;
  
  // decode an instance number from the blockIdx and threadIdx
  instance=(blockIdx.x*blockDim.x + threadIdx.x)/TPI;
  if(instance>=count)
    return;

  context_t      bn_context(cgbn_no_checks, NULL, instance);
  env_t          bn_env(bn_context.env<env_t>());      // construct an environment for 1024-bit math
  env_t::cgbn_t  a, b, t;                              // define a, b, t as 1024-bit bignums

  cgbn_load(bn_env, a, &(instances[instance].a));      // load my instance's a value
  cgbn_load(bn_env, b, &(instances[instance].b));      // load my instance's b value
  
  cgbn_add(bn_env, t, a, b);                            // t=a+b
  cgbn_add(bn_env, a, t, t);                            // a=t+t
  cgbn_add(bn_env, b, a, t);                            // b=a+t
  cgbn_add(bn_env, a, b, t);                            // a=b+t
  cgbn_add(bn_env, b, a, t);                            // b=a+t
  cgbn_add(bn_env, a, b, t);                            // a=b+t
  cgbn_add(bn_env, b, a, t);                            // b=a+t
  cgbn_add(bn_env, a, b, t);                            // a=b+t
  cgbn_add(bn_env, b, a, t);                            // b=a+t
  cgbn_add(bn_env, a, b, t);                            // a=b+t
  
  cgbn_store(bn_env, &(instances[instance].sum), a);      // store a into sum
}


/***********************/
/*** Multiply Kernel ***/
/***********************/

__global__ void kernel_mul(cgbn_error_report_t *report, instance_t *instances, uint32_t count) {
  int32_t instance;
  
  // decode an instance number from the blockIdx and threadIdx
  instance=(blockIdx.x*blockDim.x + threadIdx.x)/TPI;
  if(instance>=count)
    return;

  context_t      bn_context(cgbn_no_checks, NULL, instance);
  env_t          bn_env(bn_context.env<env_t>());      // construct an environment for 1024-bit math
  env_t::cgbn_t  a, b, r;                              // define a, b, r as 1024-bit bignums

  cgbn_load(bn_env, a, &(instances[instance].a));      // load my instance's a value
  cgbn_load(bn_env, b, &(instances[instance].b));      // load my instance's b value
  cgbn_mul(bn_env, r, a, b);                           // r=a+b
  cgbn_store(bn_env, &(instances[instance].sum), r);   // store r into sum
}

__global__ void kernel_6mul(cgbn_error_report_t *report, instance_t *instances, uint32_t count) {
  int32_t instance;
  
  // decode an instance number from the blockIdx and threadIdx
  instance=(blockIdx.x*blockDim.x + threadIdx.x)/TPI;
  if(instance>=count)
    return;

  context_t      bn_context(cgbn_no_checks, NULL, instance);
  env_t          bn_env(bn_context.env<env_t>());      // construct an environment for 1024-bit math
  env_t::cgbn_t  a, b, t;                              // define a, b, t as 1024-bit bignums

  cgbn_load(bn_env, a, &(instances[instance].a));      // load my instance's a value
  cgbn_load(bn_env, b, &(instances[instance].b));      // load my instance's b value
  cgbn_mul(bn_env, t, a, b);                           // t=a*b
  cgbn_mul(bn_env, a, t, t);                           // a=t*t
  cgbn_mul(bn_env, b, a, t);                           // b=a*t
  cgbn_mul(bn_env, a, b, t);                           // a=b*t
  cgbn_mul(bn_env, b, a, t);                           // b=a*t
  cgbn_mul(bn_env, a, b, t);                           // a=b*t

  cgbn_store(bn_env, &(instances[instance].sum), a);   // store a into sum
}

#endif // CGBN_KERNELS
