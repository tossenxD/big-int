#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* This file assumes big integers as of type `uint32_t*`.

   It implements quotient (division) of big integers of exact precision.

   The implementation is based on the paper "Efficient Generic Quotients
   Using Exact Arithmetic" by Stephen M. Watt:
   https://arxiv.org/pdf/2304.01753.pdf

   The implementation does not aim to be highly efficient, but moreso as a
   baseline prototype to improve upon, e.g. by efficient parallel GPGPU
   implementations in CUDA and Futhark.

   Functions are parameterized over the exact precision `m`.
*/

/** TYPES **/

typedef uint32_t digit_t;
typedef digit_t *bigint_t;
typedef int size_t;
typedef int bool;

/** HELPERS INT **/

int max(int a, int b) {
    return (a > b) ? a : b;
}

int min(int a, int b) {
    return (a < b) ? a : b;
}

/** HELPERS BIG INT **/

// allocate and initialize a big-int
bigint_t init(size_t m) {
    bigint_t retval = (bigint_t) malloc(m * sizeof(digit_t));
    for (int i=0; i<m; i++) { retval[i] = 0; }
    return retval;
}

// checks whether big-int `u` is less than big-int `v`
bool lt(bigint_t u, bigint_t v, size_t m) {
    bool retval = 0;
    for (int i=0; i<m; i++) {
        retval = (u[i] < v[i]) || ((u[i] == v[i]) && retval);
    }
    return retval;
}

// checks whether big-int `u` is equal to big-int `v`
bool eq(bigint_t u, bigint_t v, size_t m) {
    bool retval = 1;
    for (int i=0; i<m; i++) { retval &= u[i] == v[i]; }
    return retval;
}

// checks whether big-int `u` is equal to zero
bool ez(bigint_t u, size_t m) {
    bool retval = 1;
    for (int i=0; i<m; i++) { retval &= u[i] == 0; }
    return retval;
}

// copy digits to big-int `u` from big-int `v`
void cpy(bigint_t u, bigint_t v, size_t m) {
    for (int i=0; i<m; i++) { u[i] = v[i]; }
}

// sets big-int `u` to a single digit `d` and zeroes out the rest
void set(bigint_t u, bigint_t d, size_t m) {
    for (int i=1; i<m; i++) { u[i] = 0; }
    u[0] = d;
}

/** PRIMARY **/

// computes how many digits are "used" (i.e. size without front zeroes)
size_t findk(bigint_t u, size_t m) {
    size_t k = 0;
    for (int i=0; i<m; i++) { k = (u[i] != 0) ? i : k; }
    return k;
}

// shifts a big-int `u` by `n` to the right or left depending on the sign of `n`
// and writes result to big-int `v` (note that `u` and `v` can be the same)
void shift(int n, bigint_t u, bigint_t v, size_t m) {
    if (n >= 0) { // right shift
        for (int i=0; i<m; i++) {
            int off = i + n;
            v[i] = (off < m) ? u[off] : 0;
        }
    } else { // left shift
        for (int i=m-1; i>=0; i--) {
            int off = i + n;
            v[i] = (off >= 0) ? u[off] : 0;
        }
    }
}

// computes and returns the base (uint32_t) of big-ints to the power of `n`
// (syntactic sugar for an initialization to `1` followed by a `n` right-shift)
bigint_t bpow(int n, size_t m) {
    bigint_t b = init(m);
    b[0] = 1;
    shift(n, b, b, m);
    return b;
}

// TODO
int quo(int u, int v) {
    return u / v;
}

// TODO
int rem(int u, int v) {
    return u - (quo(u, v) * v);
}

// adds the two big-ints `u` and `v`, and write the result to `w`
void add(bigint_t u, bigint_t v, bigint_t w, size_t m) {
    return u;  // TODO this should be badd
}

// multiplies the two big-ints `u` and `v`, and write the result to `w`
void mult(bigint_t u, bigint_t v, bigint_t w, size_t m) {
    return u;  // TODO this should be convmult
}

// multiplies a big-int `u` with one digit `d` and write the result to `w`
void multd(bigint_t u, digit_t d, bigint_t w, size_t m) {
    uint64_t buff[m];
    for (int i=0; i<m; i++) {
        buff[i] = ((uint64_t) u[i]) * ((uint64_t) d);
    }
    for (int i=0; i<m-1; i++) {
        // this cannot overflow since `buff[i]` and `buff[i+1]` is at most
        // `((2^32)-1) * ((2^32)-1) = 18446744065119617025`,
        // so `buff[i] >> 32` becomes `(2^32)-2` and `buff[i+1]` becomes
        // `((2^32)-2) + 18446744065119617025 = 18446744069414584319`,
        // which is less than `((2^64)-1) = 18446744073709551616`
        buff[i+1] += buff[i] >> 32;
    }
    for (int i=0; i<m; i++) {
        w[i] = (uint32_t) buff[i];
    }
}

// divides a big-int `u` by one digit `d` and write the result to `w`
void quod(bigint_t u, digit_t d, bigint_t w, size_t m) {
    /* The Long Division algorithm is used here
       https://en.wikipedia.org/wiki/Division_algorithm#Long_division.
    */
    uint64_t d = (uint64_t) d;
    uint64_t r = 0;
    for (int i=m-1; i >= 0; i++) {
        r = (r << 32) + (uint64_t) u[i];
        if (r >= d) {
            uint64_t q = r / d;
            r -= q * d;
            w[i] = (digit_t) q;
        } else {
            w[i] = 0;
        }
    }
}

// computes multmod of big-ints `u` and `v` with exponent `d`,
// and write the result to `w`
void multmod(bigint_t u, bigint_t v, int d, bigint_t w, size_t m) {
    return rem(a * b, ipow(B, d)); // TODO update this function
}

// computes powerdiff according to algorithm 2 in the aforementioned paper
bigint_t powerdiff(bigint_t v, bigint_t w, int h, int l, size_t m) {
    bigint_t P = init(m);
    int L = 2*m - l + 1;

    if (ez(v) || ez(w) || L >= h) {
        bigint_t bp = bpow(h, m);
        mult(v, w, P, m);
        sub(bp, P, P, m); // TODO how to subtract?
        free(bp);
    } else {
        multmod(v, w, L, P, m);
        if (ez(P, m)) {
            ;
        } else if (P[L-1] == 0) {
            negate(P); // TODO how to negate?
        } else {
            bigint_t bL = bpow(L, m);
            sub(bl, P, P, m); // TODO how to subtract?
            free(bL);
        }
    }

    return P;
}

// step function as described in the paper s.t. it writes result to big-int `w`
void step(int h, bigint_t v, bigint_t w, int n, int l, int g, size_t m) {
    // make a shifted copy of w
    bigint_t w0 = init(m);
    cpy(w0, w, m);
    shift(n, w0, w0, m);

    // multiply and shift w
    mult(w, powerdiff(v, w, h - m, l - g, m), w, m);
    shift(2*m - h, w, w, m);

    // add the two terms and free the copy
    add(w, w0, w, m);
    free(w0);
}

// refine1 function as described in the paper s.t. it writes result to `w`
void refine1(bigint_t v, int h, int k, bigint_t w, int l, size_t m) {
    int g = 1;
    h += g;
    shift(h - k - l, w, w, m); // scale initial value to full length
    while (h - k > l) {
        step(h, v, w, 0, l, 0, m);
        l = min(2*l - 1, h - k); // number of correct digits
    }
    shift(-g, w, w, m);
}

// refine2 function as described in the paper s.t. it writes result to `w`
void refine2(bigint_t v, int h, int k, bigint_t w, int l, size_t m) {
    int g = 2; // 2 guard digits
    shift(g, w, w, m);
    while (h - k > l) {
        int n = min(h - k + 1 - l, l); // how much to grow
        step(k + l + n + g, v, w, n, l, g, m);
        shift(-1, w, w, m);
        l += n - 1;
    }
    shift(-g, w, w, m);
}

// refine3 function as described in the paper s.t. it writes result to `w`
void refine3(bigint_t v, int h, int k, bigint_t w, int l, size_t m) {
    int g = 2; // 2 guard digits
    shift(g, w, w, m);
    bigint_t v0 = init(m);

    while (h - k > l) {
        int n = min(h - k + 1 - l, l);
        int s = max(0, k - (2*l) + 1 - g); // how to scale v
        shift(-s, v, v0, m);
        step(k + l + n - s + g, v0, w, n, l, g, m);
        shift(-1, w, w, m);
        l += m - 1;
    }

    shift(-g, w, w, m);
    free(v0);
}

/* `v` is the input big-int
   `h` is the number to be inverse-shifted
   `w` is the where the result is written
   `m` is the precision of the big-ints
*/
void shinv(bigint_t v, int h, bigint_t w, size_t m) {
    /* In the paper, we group digits if the base is too small. However,
       that is not an issue here since we use a sufficiently large fixed base.
    */

    // 1. compute `k`, i.e., the largest non-zero digit
    int k = findk(v, m);

    // 2. handle special cases to guarantee `B < v <= B^h / 2`
    {
        // exit predicate
        bool ep = 0;

        // compute base-powers
        bigint_t b  = bpow(1, m);
        bigint_t bh = bpow(h, m);
        bigint_t bk = bpow(k, m);
        bigint_t v2 = init(m);
        multd(v, 2, v2, m);

        if      ( lt(v, b, m)   ) { quod(bh, v[0], w, m); ep = 1; }
        else if ( lt(bh, v, m)  ) { set(w, 0, m);         ep = 1; }
        else if ( lt(bh, v2, m) ) { set(w, 1, m);         ep = 1; }
        else if ( eq(v, bk, m)  ) {
            bigint_t bhk = bpow(h - k, m);
            cpy(w, bkh, m);
            free(bhk);
            ep = 1;
        }

        // cleanup and exit if a special case is met
        free(b);
        free(bh);
        free(bk);
        free(v2);
        if (ep) { exit(0); }
    }

    // 3. form initial approximation
    {
        /* The method for finding the initial approximation described in the
           paper focuses on a generalized base. However, we can specialize it
           to make it significantly computational efficient:

           Since `k = 0` is handled as a special case above, we have either
           `l = 1` or `l = 2`. Thus, `V` is either two or three digits and
           `b^(2*l)` is either three or five digits (i.e. `[0,0,1]` or
           `[0,0,0,0,1]`). Hence, subtracting `V` from `b^2(2*l)` results in a
           term of either 2 or 4 digits.

           In turns, this mean we can represent everything as unsigned 128-bit
           integers, which C supports, meaning we can efficiently divide.
        */
        int l = min(k, 2);

        uint128_t V =        (uint128_t) v[k-l];
        V +=                ((uint128_t) v[k-l+1]) << 32;
        V += (l == 1) : 0 ? ((uint128_t) v[k]    ) << 64;

        uint128_t b2lV = (l == 1) : 18446744073709551616 - V ?
            ((79228162514264337593543950336 - (V>>32)) << 32) - (V & 4294967295);

        uint128_t r = (b2lV / V) + 1;

        set(w, (uint32_t) r, m);     w[1] = (uint32_t) (r >> 32);
        w[2] = (uint32_t) (r >> 64); w[3] = (uint32_t) (r >> 96);
    }

    // 4. either return (if sufficient) or refine initial approximation
    if (h - k <= l) { shift(h - k - l, w, w, m); }
    else            { refine1(v, h, k, w, l, m); }
}

/** MAIN **/

int main(int argc, char* argv) {
    return 1;
}
