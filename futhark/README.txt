This directory contains Futhark implementations of addition, classical
multiplication, subtraction, division, shifts, and comparisons.

`add.fut` and `mul.fut` contains multiple versions of the arithmetics, as well
as callers for batch processing one and ten/six arithmetics at block level.

`div.fut` contains the main functions described in the thesis under
`Division->Algorithm`. More, it contains the helper functions (finding
assumptions, base powers, etc.) and the arithmetic methods, which it is
parameterized over. I.e. MUL, SUB, and ADD from the thesis algorithm can be
specified in their respective function of `div.fut`.

The arithmetics shares a common base type, specified in the file `helper.fut`.

Directories `test` and `bench` contains tests and benchmarks. The tests consists
of two main files, one for validation testing against GMP and one for testing of
basic properties. It also has the library `gmp-validation-lib.fut`, which is
then to be validated by GMP in the C file `gmp-validation.c`. To run all tests
for a certain base, e.g. `u64`, on both GPU and CPU, call:

  $ make test-u64

The C tests will run some tests with big integers with sizes that is a power of
2, but also some randomly picked sizes (it is printed which type is tested). The
validation and testing of properties are tested using `futhark-test`.

The benchmarks follows a similar setup. They can be called with their base type,
e.g. `u64`, in the following manner:

  $ make add-u64
  $ make mul-u64

(NOTE, there are no benchmarks for division, as it is not yet validated.) This
will run both the one- and ten/six callers for the arithmetics. They are run
using `futhark-bench`, and the runtimes are reported. The file `form-tool.awk`
used to compute metrics on the fly, but is currently borked. Instead, the
Haskell modules `BandwidthConverter.hs` and `Gu32opsConverter.hs` is attached
for convenience, but require manually inputting the runtimes as a tuple
(e.g. through an interpreter).
