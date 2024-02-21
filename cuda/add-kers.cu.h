#ifndef ADD_KERNELS
#define ADD_KERNELS

// Declare the big-integer class
template<class T>
struct bigint_t {
    typedef T t;
    uint64_t len;
    T* num;
};

template<class T>
void bigint_init (bigint_t<T>& bn, uint64_t l, T init = (T)0) {
    bn.len = l;
    bn.num = (T*) malloc(sizeof(T)*l);
    bn.num[0] = init;
    for (int i=1; i<l; i++) {
	bn.num[i] = (T)0;
    }
}

/*
  class CarryProp {
  static __device___ __host__ inline int apply(int c1, int c2) {
  (c1 & c2 & 2) | (((c1 & (c2 >> 1)) | c2) & 1)
  }
  }
*/

template<class T>
__global__ void kernel_add(bigint_t<T>* bigints_in1, bigint_t<T>* bigints_in2,
			   bigint_t<T>* bigints_out, int count) {
    const unsigned int gid = blockIdx.x * blockDim.x + threadIdx.x;
    if (gid < count) {
	bigint_t<T> n1 = bigints_in1[gid];
	bigint_t<T> n2 = bigints_in2[gid];
	uint64_t len = (n1.len < n2.len) ? n1.len : n2.len;
	len = (len < bigints_out[gid].len) ? len : (bigints_out[gid].len);
	for (int i=0; i<len; i++) {
	    bigints_out[gid].num[i] = n1.num[i] + n2.num[i];
	}
    }
}

#endif // ADD_KERNELS
