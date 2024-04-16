#ifndef CGBN_KERNELS
#define CGBN_KERNELS

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

  //context_t      bn_context(cgbn_report_monitor/*, report, instance*/);   // construct a context
  context_t      bn_context(cgbn_no_checks, NULL, instance);
  env_t          bn_env(bn_context.env<env_t>());                     // construct an environment for 1024-bit math
  env_t::cgbn_t  a, b, r;                                             // define a, b, r as 1024-bit bignums

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
  env_t          bn_env(bn_context.env<env_t>());                     // construct an environment for 1024-bit math
  env_t::cgbn_t  a, b, t1, t2;                                        // define a, b, t1, t2 as 1024-bit bignums

  cgbn_load(bn_env, a, &(instances[instance].a));      // load my instance's a value
  cgbn_load(bn_env, b, &(instances[instance].b));      // load my instance's b value
  
  cgbn_add(bn_env, t2, a,   b);                           // t2=a+b
  cgbn_add(bn_env, t1, a,  t2);                           // t1=a+t2
  cgbn_add(bn_env, t2, a,  t1);                           // t2=a+t1
  cgbn_add(bn_env, t1, a,  t2);                           // t1=a+t2
  cgbn_add(bn_env, t2, a,  t1);                           // t2=a+t1
  cgbn_add(bn_env, t1, a,  t2);                           // t1=a+t2
  cgbn_add(bn_env, t2, a,  t1);                           // t2=a+t1
  cgbn_add(bn_env, t1, a,  t2);                           // t1=a+t2
  cgbn_add(bn_env, t2, a,  t1);                           // t2=a+t1
  cgbn_add(bn_env, t1, a,  t2);                           // t1=a+t2
  
  cgbn_store(bn_env, &(instances[instance].sum), t1);     // store t1 into sum
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

  //context_t      bn_context(cgbn_report_monitor/*, report, instance*/);   // construct a context
  context_t      bn_context(cgbn_no_checks, NULL, instance);
  env_t          bn_env(bn_context.env<env_t>());                     // construct an environment for 1024-bit math
  env_t::cgbn_t  a, b, r;                                             // define a, b, r as 1024-bit bignums

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

  //context_t      bn_context(cgbn_report_monitor/*, report, instance*/);   // construct a context
  context_t      bn_context(cgbn_no_checks, NULL, instance);
  env_t          bn_env(bn_context.env<env_t>());                     // construct an environment for 1024-bit math
  env_t::cgbn_t  a, b, t1, t2;                   // define a, b, t1, t2 as 1024-bit bignums

  cgbn_load(bn_env, a, &(instances[instance].a));      // load my instance's a value
  cgbn_load(bn_env, b, &(instances[instance].b));      // load my instance's b value
  cgbn_mul(bn_env, t2, a, b);                          // t2=a*b
  cgbn_mul(bn_env, t1, a, t2);                         // t1=a*t2
  cgbn_mul(bn_env, t2, a, t1);                         // t2=a*t1
  cgbn_mul(bn_env, t1, a, t2);                         // t1=a*t2
  cgbn_mul(bn_env, t2, a, t1);                         // t2=a*t1
  cgbn_mul(bn_env, t1, a, t2);                         // t1=a*t2

  cgbn_store(bn_env, &(instances[instance].sum), t1);   // store t1 into sum
}



#endif // CGBN_KERNELS
