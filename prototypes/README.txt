This directory is meant for prototyping. C is chosen for the prototyping
language, as C gives control over all the algorithmic details and intricacies,
while also yielding a good starting point for parallelization and optimization
to CUDA and Futhark.

`div.c` contains a full-fledged prototype of a division algorithm, which, to
the extent of my knowledge, produces the correct output. It can be run either
on random inputs or fixed inputs. See `Makefile` for more detail on running the
program, and see `div.c` for more details about the algorithm and
implementation.

`mult.c` contains a starting point for a multiplication by convolution
implementation. It is not full-fledged, since there is no carry propagation and
it uses unary (1-bit) base. It is simply meant to illustrate the actual
convolution, rather than computing a multiplication. It compiles and run
straightforward.

Lastly, some prototyping tools are included as a small Haskell module in the
file `Debug.hs`. Haskell uses arbitrary precision big integers, and makes it
really easy to do the math ``by hand''. The file contains a function `fromList`
to convert our big integer representation to the one of Haskell, and `toList`
for the converse. It also implements some functions for the purpose of checking
correctness of each division subroutine while debugging. The debugging module
is meant to be loaded in an interpreter, e.g. by `$ ghci Debug.hs`.
