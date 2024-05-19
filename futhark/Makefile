# SETUP
BFLGS = --backend=cuda
BDIR  = bench/
TDIR  = test/
FORM  = | awk -f form-tool.awk -

# TESTS

.PHONY: test test-u32

test: test-u64

test-u64: test-gmp-validation-compile-u64 test-gmp-validation-run test-fut-validation-properties-u64

test-u32: test-gmp-validation-compile-u32 test-gmp-validation-run test-fut-validation-properties-u32

test-gmp-validation-compile-u64:
	@echo "## COMPILING CPU GMP-VALIDATION TESTS"
	futhark c --library $(TDIR)gmp-validation-lib.fut
	gcc -DBASE=64 $(TDIR)gmp-validation.c -o $(TDIR)gmp-validation-cpu $(TDIR)gmp-validation-lib.c -lgmp
	@echo "## COMPILING GPU GMP-VALIDATION TESTS"
	futhark opencl --library $(TDIR)gmp-validation-lib.fut
	gcc -DBASE=64 $(TDIR)gmp-validation.c -o $(TDIR)gmp-validation-gpu $(TDIR)gmp-validation-lib.c -lgmp -lOpenCL -lm

test-gmp-validation-compile-u32:
	@echo "## COMPILING CPU GMP-VALIDATION TESTS"
	futhark c --library $(TDIR)gmp-validation-lib.fut
	gcc -DBASE=32 $(TDIR)gmp-validation.c -o $(TDIR)gmp-validation-cpu $(TDIR)gmp-validation-lib.c -lgmp
	@echo "## COMPILING GPU GMP-VALIDATION TESTS"
	futhark opencl --library $(TDIR)gmp-validation-lib.fut
	gcc -DBASE=32 $(TDIR)gmp-validation.c -o $(TDIR)gmp-validation-gpu $(TDIR)gmp-validation-lib.c -lgmp -lOpenCL -lm

test-gmp-validation-run:
	@echo "## RUNNING CPU GMP-VALIDATION TESTS"
	./$(TDIR)gmp-validation-cpu
	@echo "## RUNNING GPU GMP-VALIDATION TESTS"
	./$(TDIR)gmp-validation-gpu

test-fut-validation-properties-u64:
	@echo "## RUNNING CPU FUT-VALIDATION AND FUT-PROPERTIES TESTS"
	futhark test --backend=c $(TDIR)fut-validation-u64.fut $(TDIR)fut-properties-u64.fut
	@echo "## RUNNING GPU FUT-VALIDATION AND FUT-PROPERTIES TESTS"
	futhark test --backend=opencl $(TDIR)fut-validation-u64.fut $(TDIR)fut-properties-u64.fut

test-fut-validation-properties-u32:
	@echo "## RUNNING CPU FUT-VALIDATION AND FUT-PROPERTIES TESTS"
	futhark test --backend=c $(TDIR)fut-validation-u32.fut $(TDIR)fut-properties-u32.fut
	@echo "## RUNNING GPU FUT-VALIDATION AND FUT-PROPERTIES TESTS"
	futhark test --backend=opencl $(TDIR)fut-validation-u32.fut $(TDIR)fut-properties-u32.fut

# BENCHMARKS

.PHONY: add

add: add32 add64

add32: one-add32 ten-add32

add64: one-add64 ten-add64

one-add32:
	futhark bench $(BFLGS) $(BDIR)one-add32.fut $(FORM)

ten-add32:
	futhark bench $(BFLGS) $(BDIR)ten-add32.fut $(FORM)

one-add64:
	futhark bench $(BFLGS) $(BDIR)one-add64.fut $(FORM)

ten-add64:
	futhark bench $(BFLGS) $(BDIR)ten-add64.fut $(FORM)



.PHONY: mul

mul: mul32 mul64

mul32: one-mul32 six-mul32

mul64: one-mul64 six-mul64

one-mul32:
	futhark bench $(BFLGS) $(BDIR)one-mul32.fut $(FORM)

six-mul32:
	futhark bench $(BFLGS) $(BDIR)six-mul32.fut $(FORM)

one-mul64:
	futhark bench $(BFLGS) $(BDIR)one-mul64.fut $(FORM)

six-mul64:
	futhark bench $(BFLGS) $(BDIR)six-mul64.fut $(FORM)

# CLEANUP

clean:
	find . -not -name '.' -not -name 'bench' -not -name 'test' -not -name '*.fut' -not -name '*.awk' -not -name '.gitignore' -not -name 'Makefile' -not -name 'gmp-validation.c' -not -name 'README.txt' -delete
