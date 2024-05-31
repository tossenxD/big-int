My Master's Thesis Source Code Repository.

This Thesis is all about arithmetics of big integers in parallel on
GPGPU. Following is the abstract from the thesis:

  ``Exact big integer arithmetic is a fundamental component of numerous
    scientific fields, and therefore, required to be efficient. One way to
    increase efficiency is by acceleration on GPGPU, calling for parallel
    arithmetic algorithms. This thesis examines parallel algorithms for
    addition, multiplication, and division, with the premise of fitting in a
    CUDA block, and consequently, suited for medium-sized big integers.
    The algorithms are implemented in the high-level languages C++ and
    Futhark. The addition algorithm boils down to a prefix sum, which
    runs efficiently in both implementations. The multiplication algorithm
    is the classical quadratic method, parallelized by orchestrating the
    convolutions in a way that balances the sequential work per thread
    and minimizes synchronization. The C++ implementation exhibits
    good performance, while the Futhark implementation leaves room for
    improvement. The division algorithm is based on finding multiplicative
    inverses without leaving the domain of big integers. To do so, a variety
    of big integer operators and routines are defined, including shifts,
    comparisons, and signed subtraction using the prefix sum approach
    of addition. The algorithm parameterizes over the methods involved
    for big integer arithmetic, and its efficiency directly mirrors the given
    multiplication method. In addition to conveying the algorithm, as well
    as adapting it to big integers, supplementary implementations have
    been produced. This includes a validating and inefficient sequential
    implementation in C, and a partially validating and semi-efficient
    parallel implementation in Futhark.''

This repository is structured in four directories; `cuda` and `futhark` contains
the bulk of the product of the Thesis, i.e., all the parallel code and
implementations in the respective language. `prototypes` contains sequential
prototypes written in C, which have served the purpose to familiarize algorithms
before converting them to a parallel version. 'thesis' contains the source code
of the thesis.

The four directories contains their own README's, detailing what they contain
and how to run them. The code is generally well-commented, and should speak for
itself, but the thesis displays a more in-depth look of the designs of the
algorithms - both their sequential intuitions and parallel augmentations.

The thesis was written over a period of four months from 31th of January 2024 to
31th of May 2024.
