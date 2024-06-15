#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <gmp.h>
#include "gmp-validation-lib.h"

#define RUNS_PER_SIZE 50

/* Requires `BASE` to be defined externally.
   Supports 16, 32, and 64 bits (which is default).
*/
#ifndef BASE
#define BASE 64
#endif

#if BASE==16
typedef uint16_t uint_t;
#endif
#if BASE==32
typedef uint32_t uint_t;
#endif
#if BASE==64
typedef uint64_t uint_t;
#endif

/* Runs big integer arithmetics using GMP. */
void run_gmp(uint_t m, uint_t *u, uint_t *v, uint_t *w_add,
             uint_t *w_sub, uint_t *w_mul, uint_t *w_div) {
    mpz_t a; mpz_init(a); mpz_import(a, m, -1, sizeof(uint_t), 0, 0, u);
    mpz_t b; mpz_init(b); mpz_import(b, m, -1, sizeof(uint_t), 0, 0, v);
    mpz_t r; mpz_init(r);
    size_t countp;

    mpz_add(r, a, b);
    mpz_export(w_add, &countp, -1, sizeof(uint_t), 0, 0, r);
    while(countp < m) { w_add[countp++] = 0; }

    mpz_sub(r, a, b);
    mpz_abs(r, r);
    mpz_export(w_sub, &countp, -1, sizeof(uint_t), 0, 0, r);
    while(countp < m) { w_sub[countp++] = 0; }

    mpz_mul(r, a, b);
    mpz_export(w_mul, &countp, -1, sizeof(uint_t), 0, 0, r);
    while(countp < m) { w_mul[countp++] = 0; }

    mpz_div(r, a, b);
    mpz_export(w_div, &countp, -1, sizeof(uint_t), 0, 0, r);
    while(countp < m) { w_div[countp++] = 0; }

    mpz_clear(a); mpz_clear(b); mpz_clear(r);
}

/* Generates a random unsigned integer. */
uint_t rand_uint_t() {
    uint_t low = rand()*2;
    uint_t high = rand()*2;
    return (high << (BASE / 2)) + low;
}

/* Fills big integer `u` of `m * BASE` bits with `n` non-zero digits. */
void rand_bigint(int m, int n, uint_t *u) {
    for (int i = 0; i < m; i++) {
        u[i] = (i < n) ? rand_uint_t() : 0;
    }
}

/* Test for equality of two big integers. */
int eq(int m, uint_t *u, uint_t *v) {
    int retval = 1;
    for(int i=0; i<m; i++) { retval &= u[i] == v[i]; }
    return retval;
}

/* Tests the Futhark arithemtic functions exported by `gmp-validation-lib.fut`
   against the ones implemented in GMP on random inputs.
*/
void run_tests(uint_t m_init, int m_grwth, uint32_t runs, struct futhark_context *ctx) {
    // keep track of passed tests
    int addC = 0; int subC = 0; int mulC = 0; int divC = 0;
    for (uint32_t i = 0, m = m_init; i < runs;
         i++, m = (m_grwth < 0) ? rand_uint_t() % 511 + 2 : m * m_grwth) {

        for (int j = 0; j < RUNS_PER_SIZE; j++) {
            // create some random input arrays
            uint_t *u = (uint_t *) malloc(m * sizeof(uint_t));
            uint_t *v = (uint_t *) malloc(m * sizeof(uint_t));

            rand_bigint(m, rand_uint_t() % m + 1, u);
            rand_bigint(m, rand_uint_t() % m + 1, v);

            #if BASE==16
            struct futhark_u16_1d *u_fut = futhark_new_u16_1d(ctx, u, m);
            struct futhark_u16_1d *v_fut = futhark_new_u16_1d(ctx, v, m);
            #endif

            #if BASE==32
            struct futhark_u32_1d *u_fut = futhark_new_u32_1d(ctx, u, m);
            struct futhark_u32_1d *v_fut = futhark_new_u32_1d(ctx, v, m);
            #endif

            #if BASE==64
            struct futhark_u64_1d *u_fut = futhark_new_u64_1d(ctx, u, m);
            struct futhark_u64_1d *v_fut = futhark_new_u64_1d(ctx, v, m);
            #endif

            futhark_context_sync(ctx);

            // allocate output arrays
            uint_t *w_gmp_add = (uint_t *) malloc((m+1) * sizeof(uint_t));
            uint_t *w_gmp_sub = (uint_t *) malloc( m    * sizeof(uint_t));
            uint_t *w_gmp_mul = (uint_t *) malloc( 2*m  * sizeof(uint_t));
            uint_t *w_gmp_div = (uint_t *) malloc( m    * sizeof(uint_t));

            uint_t *w_our_add = (uint_t *) malloc(m * sizeof(uint_t));
            uint_t *w_our_sub = (uint_t *) malloc(m * sizeof(uint_t));
            uint_t *w_our_mul = (uint_t *) malloc(m * sizeof(uint_t));
            uint_t *w_our_div = (uint_t *) malloc(m * sizeof(uint_t));
            for (uint_t k=0; k<m; k++) { w_our_add[k] = 0; w_our_sub[k] = 0;
                                         w_our_mul[k] = 0; w_our_div[k] = 0;
            }

            #if BASE==16
            struct futhark_u16_1d *w_fut_add = futhark_new_u16_1d(ctx, w_our_add, m);
            struct futhark_u16_1d *w_fut_sub = futhark_new_u16_1d(ctx, w_our_sub, m);
            struct futhark_u16_1d *w_fut_mul = futhark_new_u16_1d(ctx, w_our_mul, m);
            struct futhark_u16_1d *w_fut_div = futhark_new_u16_1d(ctx, w_our_div, m);
            #endif

            #if BASE==32
            struct futhark_u32_1d *w_fut_add = futhark_new_u32_1d(ctx, w_our_add, m);
            struct futhark_u32_1d *w_fut_sub = futhark_new_u32_1d(ctx, w_our_sub, m);
            struct futhark_u32_1d *w_fut_mul = futhark_new_u32_1d(ctx, w_our_mul, m);
            #endif

            #if BASE==64
            struct futhark_u64_1d *w_fut_add = futhark_new_u64_1d(ctx, w_our_add, m);
            struct futhark_u64_1d *w_fut_sub = futhark_new_u64_1d(ctx, w_our_sub, m);
            struct futhark_u64_1d *w_fut_mul = futhark_new_u64_1d(ctx, w_our_mul, m);
            #endif

            futhark_context_sync(ctx);

            // run arithmetics using GMP
            run_gmp(m, u, v, w_gmp_add, w_gmp_sub, w_gmp_mul, w_gmp_div);

            // run arithmetics using Futhark
            futhark_entry_test_add(ctx, &w_fut_add, u_fut, v_fut);
            futhark_entry_test_sub(ctx, &w_fut_sub, u_fut, v_fut);
            futhark_entry_test_mul(ctx, &w_fut_mul, u_fut, v_fut);
            #if BASE==16
            futhark_entry_test_div(ctx, &w_fut_div, u_fut, v_fut);
            #endif

            futhark_context_sync(ctx);

            #if BASE==16
            futhark_values_u16_1d(ctx, w_fut_add, w_our_add);
            futhark_values_u16_1d(ctx, w_fut_sub, w_our_sub);
            futhark_values_u16_1d(ctx, w_fut_mul, w_our_mul);
            futhark_values_u16_1d(ctx, w_fut_div, w_our_div);
            #endif

            #if BASE==32
            futhark_values_u32_1d(ctx, w_fut_add, w_our_add);
            futhark_values_u32_1d(ctx, w_fut_sub, w_our_sub);
            futhark_values_u32_1d(ctx, w_fut_mul, w_our_mul);
            #endif

            #if BASE==64
            futhark_values_u64_1d(ctx, w_fut_add, w_our_add);
            futhark_values_u64_1d(ctx, w_fut_sub, w_our_sub);
            futhark_values_u64_1d(ctx, w_fut_mul, w_our_mul);
            #endif

            futhark_context_sync(ctx);

            // compare Futhark and GMP results
            if (eq(m, w_gmp_add, w_our_add)) { addC++; }
            if (eq(m, w_gmp_sub, w_our_sub)) { subC++; }
            if (eq(m, w_gmp_mul, w_our_mul)) { mulC++; }
            #if BASE==16
            if (eq(m, w_gmp_div, w_our_div)) { divC++; }
            #endif

            // cleanup
            free(u);
            free(v);
            free(w_gmp_add);
            free(w_gmp_sub);
            free(w_gmp_mul);
            free(w_gmp_div);
            free(w_our_add);
            free(w_our_sub);
            free(w_our_mul);
            free(w_our_div);

            #if BASE==16
            futhark_free_u16_1d(ctx, u_fut);
            futhark_free_u16_1d(ctx, v_fut);
            futhark_free_u16_1d(ctx, w_fut_add);
            futhark_free_u16_1d(ctx, w_fut_sub);
            futhark_free_u16_1d(ctx, w_fut_mul);
            futhark_free_u16_1d(ctx, w_fut_div);
            #endif

            #if BASE==32
            futhark_free_u32_1d(ctx, u_fut);
            futhark_free_u32_1d(ctx, v_fut);
            futhark_free_u32_1d(ctx, w_fut_add);
            futhark_free_u32_1d(ctx, w_fut_sub);
            futhark_free_u32_1d(ctx, w_fut_mul);
            #endif

            #if BASE==64
            futhark_free_u64_1d(ctx, u_fut);
            futhark_free_u64_1d(ctx, v_fut);
            futhark_free_u64_1d(ctx, w_fut_add);
            futhark_free_u64_1d(ctx, w_fut_sub);
            futhark_free_u64_1d(ctx, w_fut_mul);
            #endif
        }
    }
    printf("add:\t[%d/%d] PASSED\n", addC, RUNS_PER_SIZE*runs);
    printf("sub:\t[%d/%d] PASSED\n", subC, RUNS_PER_SIZE*runs);
    printf("mul:\t[%d/%d] PASSED\n", mulC, RUNS_PER_SIZE*runs);
    #if BASE==16
    printf("div:\t[%d/%d] PASSED\n", divC, RUNS_PER_SIZE*runs);
    #endif
}

/* Runs the test suite on different random input size patterns. */
int main() {
    // create an Futhark configuration and context
    struct futhark_context_config *cfg = futhark_context_config_new();
    struct futhark_context *ctx = futhark_context_new(cfg);

    // run tests
    printf("TESTS WITH RANDOM INPUTS OF SIZE THAT IS A MULTIPLE OF 4:\n");
    run_tests(4, 2, 7, ctx);

    printf("\nTESTS WITH RANDOM INPUTS OF SPORADIC SIZES:\n");
    run_tests(2, -1, 50, ctx);

    // cleanup
    futhark_context_free(ctx);
    futhark_context_config_free(cfg);
}
