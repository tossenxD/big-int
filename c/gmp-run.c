#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <gmp.h>

void gmpRun(int m, int t, uint32_t* as, uint32_t* bs, uint32_t* rs) {
    mpz_t a; mpz_t b; mpz_t r;
    mpz_init(a); mpz_init(b); mpz_init(r);

    mpz_import(a, m, -1, sizeof(uint32_t), 0, 0, as);
    mpz_import(b, m, -1, sizeof(uint32_t), 0, 0, bs);

    if (t) {
        mpz_mul(r, a, b);
    } else {
        mpz_div(r, a, b);
    }

    mpz_export(rs, NULL, -1, sizeof(uint32_t), 0, 0, r);
}

int main(int argc, char* argv[]) {
    if (argc < 4) {
        printf("Usage: %s <0/1> <m> <space-seperated big integers>\n", argv[0]);
        exit(1);
    }

    int t = atoi(argv[1]);
    int m = atoi(argv[2]);
    
    uint32_t *as = (uint32_t*) malloc(m * sizeof(uint32_t));
    uint32_t *bs = (uint32_t*) malloc(m * sizeof(uint32_t));
    uint32_t *rs = (uint32_t*) malloc(m * sizeof(uint32_t));

    for (int i=0; i < m; i++) {
        as[i] = atoi(argv[3     + i]);
        bs[i] = atoi(argv[3 + m + i]);
        rs[i] = 0;
    }

    gmpRun(m, t, as, bs, rs);

    for (int i=0; i < m; i++) {
        printf("%u ", rs[i]);
    }   printf("\n");

    return 0;
}
