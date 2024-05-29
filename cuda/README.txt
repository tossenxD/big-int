This directory contains the CUDA implementations for addition and classical
multiplication, written in file `ker-add.cu.h` and `ker-mul.cu.h`, respectively.

The kernels are structured s.t. they have two callers (for running one instance
of the arithmetic and `p` instances of the arithmetics). Furthermore, they have
multiple versions with a gradual degree of optimizations.

To call the benchmarks or tests of the kernels, use the supplied Makefile and
main file. The Makefile have two calls:
  $ make default
  $ make small
They run the tests/benchmarks with a big or small workload, respectively.

The main file `main.cu` has a lot of definitions in its header, together with a
table explaining the different parameters. Those parameters specify which kernel
versions to run, how many times to run them, the base type, etc. More detailed
descriptions of kernels, tests, and benchmarks, are in the header of the main
file as well.

The files `helper.h` and `ker-helpers.cu.h contains host and device helpers,
respectively.

Lastly, the sub-directory `cgbn` is meant for running the benchmarks with
CGBN. It has a similar setup as this top-directory. First time the CGBN is run,
it must be downloaded. To do so using the Makefile, write:
  $ make cgbn-prep
In addition to downloading CGBN, this command makes a small patch (using the
program `sed`) to make CGBNs own tests run (s.t. it can easily be verified to
work). Then, to run all the CGBN benchmarks, call:
  $ make run-all
