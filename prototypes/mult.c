#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int main(int argc, char *argv) {
    int n = 8;    // number of bits
    uint8_t a[n]; // actually each cell is 1 bit - 8 used for ease of debugging
    uint8_t b[n]; 
    uint8_t c[n];

    // initialize a and b to 0b1111 (in decimal, 15)
    for (int k = 0; k < n; k++) { a[k] = k < 4; }
    for (int k = 0; k < n; k++) { b[k] = k < 4; }

    printf("A:\t\t");
    for (int k = 0; k < n; k++) { printf("%d ", a[k]); }
    printf("\nB:\t\t");
    for (int k = 0; k < n; k++) { printf("%d ", b[k]); }
    printf("\n");

    
    /****************************************************/
    for (int k = 0; k < n; k++) { c[k] = 0; }
    
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            int r = a[i] * b[j];
            int l = r % 10;
            int h = r / 10;
            if (i+j   < n) { c[i+j]   += l; }
            if (i+j+1 < n) { c[i+j+1] += h; }
        }
    }

    printf("Sequential:\t");
    for (int k = 0; k < n; k++) { printf("%d ", c[k]); }
    printf("\n");

    
    /****************************************************/
    uint8_t ls[n];
    uint8_t hs[n];
    uint8_t cs[n];

    for (int k = 0; k < n; k++) { c[k] = 0; ls[k] = 0; hs[k] = 0; cs[k] = 0; }

    for (int tid = 0; tid < n/2; tid++) { // thread
        int k1 = tid;
        for (int i = 0; i <= k1; i++) {
            int j = k1 - i;
            uint16_t r = a[i] * b[j];
            uint8_t h = 1 & (r >> 1);
            uint8_t l = (uint8_t) (r & 1);
            ls[k1] = (ls[k1] + l) & 1;
            uint8_t tmp = hs[k1];
            hs[k1] += (h + (ls[k1] < l)) & 1;
            cs[k1] += (hs[k1] < tmp);
        }
        int k2 = n-1 - tid;
        for (int i = 0; i <= k2; i++) {
            int j = k2 - i;
            uint16_t r = a[i] * b[j];
            uint8_t h = 1 & (r >> 1);
            uint8_t l = (uint8_t) (r & 1);
            ls[k2] = (ls[k2] + l) & 1;
            uint8_t tmp = hs[k2];
            hs[k2] += (h + (ls[k2] < l)) & 1;
            cs[k2] += (hs[k2] < tmp);
        }
    }

    for (int tid = 0; tid < n/2; tid++) { // thread
        for (int i = 0; i < 2; i++) {
            uint8_t l = ls[tid*2 + i];
            uint8_t h = (tid) ? hs[tid*2 + i - 1] : 0;
            uint8_t cc = (tid > 1) ? cs[tid*2 + i - 2] : 0;
            uint8_t r = l + h;
            uint8_t rr = (uint8_t) (r < l);
            r += cc;
            rr += (uint8_t) (r < cc);
            c[tid*2+i] += r;
            if (tid != (n/2) - 1) {
                c[tid*2+i+1] += rr;
            }
        }
    }
    
    printf("Parallel1:\t");
    for (int k = 0; k < n; k++) { printf("%d ", c[k]); }
    printf("\n");
    printf("Ls:\t\t");
    for (int k = 0; k < n; k++) { printf("%d ", ls[k]); }
    printf("\n");
    printf("Hs:\t\t");
    for (int k = 0; k < n; k++) { printf("%d ", hs[k]); }
    printf("\n");
}
