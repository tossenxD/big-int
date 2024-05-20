#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <gmp.h>
#include "gmp-validation-lib.h"

#define RUNS_PER_SIZE 50

/* Requires `BASE` to be defined externally. Supports 16, 32, and 64 bits.
*/

/* Runs big integer arithmetics using GMP. */
void run_gmp(uint32_t m, uint32_t *u, uint32_t *v, uint32_t *w_add,
             uint32_t *w_sub, uint32_t *w_mul, uint32_t *w_div) {
    mpz_t a; mpz_init(a); mpz_import(a, m, -1, sizeof(uint32_t), 0, 0, u);
    mpz_t b; mpz_init(b); mpz_import(b, m, -1, sizeof(uint32_t), 0, 0, v);
    mpz_t r; mpz_init(r);
    size_t countp;

    mpz_add(r, a, b);
    mpz_export(w_add, &countp, -1, sizeof(uint32_t), 0, 0, r);
    while(countp < m) { w_add[countp++] = 0; }

    mpz_sub(r, a, b);
    mpz_abs(r, r);
    mpz_export(w_sub, &countp, -1, sizeof(uint32_t), 0, 0, r);
    while(countp < m) { w_sub[countp++] = 0; }

    mpz_mul(r, a, b);
    mpz_export(w_mul, &countp, -1, sizeof(uint32_t), 0, 0, r);
    while(countp < m) { w_mul[countp++] = 0; }

    mpz_div(r, a, b);
    mpz_export(w_div, &countp, -1, sizeof(uint32_t), 0, 0, r);
    while(countp < m) { w_div[countp++] = 0; }

    mpz_clear(a); mpz_clear(b); mpz_clear(r);
}

/* Generates a random 32-bit unsigned integer. */
uint32_t rand_uint32_t() {
    uint32_t low = rand()*2;
    uint32_t high = rand()*2;
    return (high << 16) + low;
}

/* Fills big integer `u` of bit-size `m * 32` with `n` non-zero digits. */
void rand_bigint(int m, int n, uint32_t *u) {
    for (int i = 0; i < m; i++) {
        u[i] = (i < n) ? rand_uint32_t() : 0;
    }
}

/* Test for equality of two big integers. */
int eq(int m, uint32_t *u, uint32_t *v) {
    int retval = 1;
    for(int i=0; i<m; i++) { retval &= u[i] == v[i]; }
    return retval;
}

/* Tests the Futhark arithemtic functions exported by `gmp-validation-lib.fut`
   against the ones implemented in GMP on random inputs.
*/
void run_tests(uint32_t m_init, int m_grwth, uint32_t runs, struct futhark_context *ctx) {
    // keep track of passed tests
    int addC = 0; int subC = 0; int mulC = 0; int divC = 0;
    for (uint32_t i = 0, m = m_init; i < runs;
         i++, m = (m_grwth < 0) ? rand_uint32_t() % 511 + 2 : m * m_grwth) {

        for (int j = 0; j < RUNS_PER_SIZE; j++) {
            // create some random input arrays
            uint32_t *u = (uint32_t *) malloc(2*m * sizeof(uint32_t));
            uint32_t *v = (uint32_t *) malloc(2*m * sizeof(uint32_t));

            rand_bigint(2*m, rand_uint32_t() % (2*m) + 1, u);
            rand_bigint(2*m, rand_uint32_t() % (2*m) + 1, v);

            uint16_t *u_16 = (uint16_t *) u;
            uint16_t *v_16 = (uint16_t *) v;
            uint32_t *u_32 = u;
            uint32_t *v_32 = v;
            uint64_t *u_64 = (uint64_t *) u;
            uint64_t *v_64 = (uint64_t *) v;

            #if BASE==16
            struct futhark_u16_1d *u_fut_16 = futhark_new_u16_1d(ctx, u_16, m);
            struct futhark_u16_1d *v_fut_16 = futhark_new_u16_1d(ctx, v_16, m);
            #endif

            #if BASE==32
            struct futhark_u32_1d *u_fut_32 = futhark_new_u32_1d(ctx, u_32, m);
            struct futhark_u32_1d *v_fut_32 = futhark_new_u32_1d(ctx, v_32, m);
            #endif

            #if BASE==64
            struct futhark_u64_1d *u_fut_64 = futhark_new_u64_1d(ctx, u_64, m);
            struct futhark_u64_1d *v_fut_64 = futhark_new_u64_1d(ctx, v_64, m);
            #endif

            futhark_context_sync(ctx);

            // allocate output arrays
            uint32_t *w_gmp_add = (uint32_t *) malloc((2*m+1) * sizeof(uint32_t));
            uint32_t *w_gmp_sub = (uint32_t *) malloc(2*m * sizeof(uint32_t));
            uint32_t *w_gmp_mul = (uint32_t *) malloc(4*m * sizeof(uint32_t));
            uint32_t *w_gmp_div = (uint32_t *) malloc(2*m * sizeof(uint32_t));

            uint32_t *w_our = (uint32_t *) malloc(2*m * sizeof(uint32_t));
            for (uint32_t j=0; j<2*m; j++) { w_our[j] = 0; }

            uint16_t *w_our_16 = (uint16_t *) w_our;
            uint32_t *w_our_32 = w_our;
            uint64_t *w_our_64 = (uint64_t *) w_our;

            #if BASE==16
            struct futhark_u16_1d *w_fut_add_16 = futhark_new_u16_1d(ctx, w_our_16, m);
            struct futhark_u16_1d *w_fut_sub_16 = futhark_new_u16_1d(ctx, w_our_16, m);
            struct futhark_u16_1d *w_fut_mul_16 = futhark_new_u16_1d(ctx, w_our_16, m);
            struct futhark_u16_1d *w_fut_div_16 = futhark_new_u16_1d(ctx, w_our_16, m);
            #endif

            #if BASE==32
            struct futhark_u32_1d *w_fut_add_32 = futhark_new_u32_1d(ctx, w_our_32, m);
            struct futhark_u32_1d *w_fut_sub_32 = futhark_new_u32_1d(ctx, w_our_32, m);
            struct futhark_u32_1d *w_fut_mul_32 = futhark_new_u32_1d(ctx, w_our_32, m);
            #endif

            #if BASE==64
            struct futhark_u64_1d *w_fut_add_64 = futhark_new_u64_1d(ctx, w_our_64, m);
            struct futhark_u64_1d *w_fut_sub_64 = futhark_new_u64_1d(ctx, w_our_64, m);
            struct futhark_u64_1d *w_fut_mul_64 = futhark_new_u64_1d(ctx, w_our_64, m);
            #endif

            futhark_context_sync(ctx);

            // run arithmetics using GMP
            run_gmp(m, u_32, v_32, w_gmp_add, w_gmp_sub, w_gmp_mul, w_gmp_div);

            // run arithmetics using Futhark
            #if BASE==16
            futhark_entry_test_add(ctx, &w_fut_add_16, u_fut_16, v_fut_16);
            futhark_entry_test_sub(ctx, &w_fut_sub_16, u_fut_16, v_fut_16);
            futhark_entry_test_mul(ctx, &w_fut_mul_16, u_fut_16, v_fut_16);
            futhark_entry_test_div(ctx, &w_fut_div_16, u_fut_16, v_fut_16);
            #endif

            #if BASE==32
            futhark_entry_test_add(ctx, &w_fut_add_32, u_fut_32, v_fut_32);
            futhark_entry_test_sub(ctx, &w_fut_sub_32, u_fut_32, v_fut_32);
            futhark_entry_test_mul(ctx, &w_fut_mul_32, u_fut_32, v_fut_32);
            #endif

            #if BASE==64
            futhark_entry_test_add(ctx, &w_fut_add_64, u_fut_64, v_fut_64);
            futhark_entry_test_sub(ctx, &w_fut_sub_64, u_fut_64, v_fut_64);
            futhark_entry_test_mul(ctx, &w_fut_mul_64, u_fut_64, v_fut_64);
            #endif

            futhark_context_sync(ctx);

            // compare Futhark and GMP results
            #if BASE==16
            futhark_values_u16_1d(ctx, w_fut_add_16, w_our_16);
            futhark_context_sync(ctx);
            if (eq(m/2, w_gmp_add, w_our)) { addC++; }

            futhark_values_u16_1d(ctx, w_fut_sub_16, w_our_16);
            futhark_context_sync(ctx);
            if (eq(m/2, w_gmp_sub, w_our)) { subC++; }

            futhark_values_u16_1d(ctx, w_fut_mul_16, w_our_16);
            futhark_context_sync(ctx);
            if (eq(m/2, w_gmp_mul, w_our)) { mulC++; }

            futhark_values_u16_1d(ctx, w_fut_div_16, w_our_16);
            futhark_context_sync(ctx);
            if (eq(m/2, w_gmp_div, w_our)) { divC++; }
            #endif

            #if BASE==32
            futhark_values_u32_1d(ctx, w_fut_add_32, w_our_32);
            futhark_context_sync(ctx);
            if (eq(m, w_gmp_add, w_our)) { addC++; }

            futhark_values_u32_1d(ctx, w_fut_sub_32, w_our_32);
            futhark_context_sync(ctx);
            if (eq(m, w_gmp_sub, w_our)) { subC++; }

            futhark_values_u32_1d(ctx, w_fut_mul_32, w_our_32);
            futhark_context_sync(ctx);
            if (eq(m, w_gmp_mul, w_our)) { mulC++; }
            #endif

            #if BASE==64
            futhark_values_u64_1d(ctx, w_fut_add_64, w_our_64);
            futhark_context_sync(ctx);
            if (eq(m, w_gmp_add, w_our)) { addC++; }

            futhark_values_u64_1d(ctx, w_fut_sub_64, w_our_64);
            futhark_context_sync(ctx);
            if (eq(m, w_gmp_sub, w_our)) { subC++; }

            futhark_values_u64_1d(ctx, w_fut_mul_64, w_our_64);
            futhark_context_sync(ctx);
            if (eq(m, w_gmp_mul, w_our)) { mulC++; }
            #endif

            // cleanup
            free(u);
            free(v);
            free(w_gmp_add);
            free(w_gmp_sub);
            free(w_gmp_mul);
            free(w_gmp_div);
            free(w_our);

            #if BASE==16
            futhark_free_u16_1d(ctx, u_fut_16);
            futhark_free_u16_1d(ctx, v_fut_16);
            futhark_free_u16_1d(ctx, w_fut_add_16);
            futhark_free_u16_1d(ctx, w_fut_sub_16);
            futhark_free_u16_1d(ctx, w_fut_mul_16);
            futhark_free_u16_1d(ctx, w_fut_div_16);
            #endif

            #if BASE==32
            futhark_free_u32_1d(ctx, u_fut_32);
            futhark_free_u32_1d(ctx, v_fut_32);
            futhark_free_u32_1d(ctx, w_fut_add_32);
            futhark_free_u32_1d(ctx, w_fut_sub_32);
            futhark_free_u32_1d(ctx, w_fut_mul_32);
            #endif

            #if BASE==64
            futhark_free_u64_1d(ctx, u_fut_64);
            futhark_free_u64_1d(ctx, v_fut_64);
            futhark_free_u64_1d(ctx, w_fut_add_64);
            futhark_free_u64_1d(ctx, w_fut_sub_64);
            futhark_free_u64_1d(ctx, w_fut_mul_64);
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
    #ifdef BASE
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
    #endif
}
