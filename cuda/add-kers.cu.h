#ifndef ADD_KERNELS
#define ADD_KERNELS

// Declare the big-integer class
template<class T>
struct bigint_t {
    typedef T t;
    uint8_t len;
    uint8_t* info; // multi-purpose information byte
    T* num;
};

template<class T>
void bigint_init (bigint_t<T>& bn, uint8_t l, T init = (T)0) {
    bn.len = l;
    bn.info = (uint8_t*) malloc(sizeof(uint8_t)*l);
    bn.num = (T*) malloc(sizeof(T)*l);
    bn.info[0] = 0;
    bn.num[0] = init;
    for (int i=1; i<l; i++) {
	bn.num[i] = (T)0;
	bn.info[i] = 0;
    }
}

// Kernel for adding two arrays of big integers
template<class T>
__global__ void kernel_add(bigint_t<T>* bigints_in1, bigint_t<T>* bigints_in2,
			   bigint_t<T>* bigints_out, int count) {
    const unsigned int gid = blockIdx.x * blockDim.x + threadIdx.x;
    if (gid < count) {
	bigint_t<T> n1 = bigints_in1[gid];
	bigint_t<T> n2 = bigints_in2[gid];
	bigint_t<T> n3 = bigints_out[gid];
	uint8_t len = (n1.len < n2.len) ? n1.len : n2.len;
	len = (len < n3.len) ? len : (n3.len);
	for (int i=0; i<len; i++) {
	    T s = n1.num[i] + n2.num[i];
	    n3.num[i] = s;
	    n3.info[i] = (s < n1.num[i]) | ((s == (T)-1) << 1);
	}
    }
}

#endif // ADD_KERNELS
