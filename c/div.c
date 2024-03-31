#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/** HELPERS **/

int max(int a, int b) {
    return (a > b) ? a : b;
}

int min(int a, int b) {
    return (a < b) ? a : b;
}

int abs(int a) {
    return (a < 0) ? - a : a;
}

int ipow(int a, int n) {
    int retval = n ? a : 1;
    for (int i=1; i<abs(n); i++) {
        retval *= a;
    }
    return retval;
}

int ilog(int B, int u) {
    return (int) (log((double) u) / log((double) B));
}


/** PRIMARY **/

int shift(int n, int B, int u) {
    int p = ipow(B, n);
    return (0 <= n) ? u * p : u / p;
}

int prec(int B, int u) {
    return ilog(B, abs(u)) + 1;
}

int quo(int u, int v) { // TODO theorem 1
    return u / v;
}

int rem(int u, int v) {
    return u - (quo(u, v) * v);
}

int mult(int a, int b) {
    return a * b;
}

int multmod(int a, int b, int d, int B) {
    return rem(a * b, ipow(B, d));
}

int powerdiff(v, w, h, l, B) {
    int L = prec(B, v) + prev(B, w) - l + 1;
    if (v == 0 || w == 0 || L >= h) {
        return ipow(B, h) - mult(v, w);
    } else {
        int P = multmod(v, w, L, B);
        if (P == 0) {
            return 0;
        } else if (P(L-1) == 0) { // TODO what does this mean?
            return -P;
        } else {
            return ipow(B, L) - P;
        }
    }
}

int step(int h, int v, int w, int m, int l, int g, int B) {
    return shift(m, B, w) +
        shift(2*m-h, B, mult(w, powerdiff(v, w, h - m, l - g, B)));
}

int refine1(int v, int h, int k, int w, int l, int B) {
    int g = 1;
    int h = h + g;
    int w = shift(h - k - l, B, w); // Scale initial value to full length
    while (h - k > l) {
        w = step(h, v, w, 0, l, 0, B);
        l = min(2*l - 1, h - k); // Number of correct digits
    }
    return shift(-g, B, w);
}

int refine2(int v, int h, int k, int w, int l) {
    return 1;
}

int refine3(int v, int h, int k, int w, int l) {
    return 1;
}

// TODO find k?
int shinv(int v, int h, int B) {
    // Group digits if base is small
    if (B < 16) {
        int p = max(6 - B, 2);
        return shift(h * rem(p) - p, B, shinv(v, quo(h, p + 1), pow(B, p)));
    }

    // Special cases guarantee B < v <= B^h / 2
    if (v < B) { return quo(ipow(B, h), v); } // Divide by 1 digit
    if (v > ipow(B, h)) { return 0; }
    if (2*v > ipow(B,h)) { return 1; }
    if (v == ipow(B, k)) { return ipow(B, h-k); }

    // Form initial approximation, returning it if sufficient
    int l = min(k, 2);
    int V = 0;
    for (int i=0; i<l; i++) { V += v(k-l+i) * ipow(B, i); } // TODO what is v_()?
    int w = quo(ipow(B, 2*l) - V, V+1); // Divide 4 digits by 2 digits
    if (h - k <= l) { return shift(h - k - l, B, w); }

    // Refine iteratively using one of the methods below
    return refine1(v, h, k, w, l, B);
}

/** MAIN **/

int main(int argc, char* argv) {
    return 1;
}
