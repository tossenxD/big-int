# SETUP
BFLGS = --backend=cuda
BDIR  = bench/
TDIR  = test/
FORM  = #| awk -f form-tool.awk -

# TESTS

.PHONY: test test-u64 test-u32 test-u16

test: test-u64

test-u64: test-gmp-validation-compile-u64 test-gmp-validation-run test-fut-validation-properties-u64

test-u32: test-gmp-validation-compile-u32 test-gmp-validation-run test-fut-validation-properties-u32

test-u16: test-gmp-validation-compile-u16 test-gmp-validation-run test-fut-validation-properties-u16

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

test-gmp-validation-compile-u16:
	@echo "## COMPILING CPU GMP-VALIDATION TESTS"
	futhark c --library $(TDIR)gmp-validation-lib.fut
	gcc -DBASE=16 $(TDIR)gmp-validation.c -o $(TDIR)gmp-validation-cpu $(TDIR)gmp-validation-lib.c -lgmp
	@echo "## COMPILING GPU GMP-VALIDATION TESTS"
	futhark opencl --library $(TDIR)gmp-validation-lib.fut
	gcc -DBASE=16 $(TDIR)gmp-validation.c -o $(TDIR)gmp-validation-gpu $(TDIR)gmp-validation-lib.c -lgmp -lOpenCL -lm

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

test-fut-validation-properties-u16:
	@echo "## RUNNING CPU FUT-VALIDATION AND FUT-PROPERTIES TESTS"
	futhark test --backend=c $(TDIR)fut-validation-u16.fut $(TDIR)fut-properties-u16.fut
	@echo "## RUNNING GPU FUT-VALIDATION AND FUT-PROPERTIES TESTS"
	futhark test --backend=opencl $(TDIR)fut-validation-u16.fut $(TDIR)fut-properties-u16.fut

# BENCHMARKS

.PHONY: add mul add-u64 add-u32 mul-u64 mul-u32

add: add-u64

mul: mul-u64

add-u64:
	futhark bench $(BFLGS) $(BDIR)add-u64.fut $(FORM)

add-u32:
	futhark bench $(BFLGS) $(BDIR)add-u32.fut $(FORM)

mul-u64:
	futhark bench $(BFLGS) $(BDIR)mul-u64.fut $(FORM)

mul-u32:
	futhark bench $(BFLGS) $(BDIR)mul-u32.fut $(FORM)

# CLEANUP

clean:
	find . -not -name '.' -not -name 'bench' -not -name 'test' -not -name '*.fut' -not -name '*.awk' -not -name '.gitignore' -not -name 'Makefile' -not -name 'gmp-validation.c' -not -name 'README.txt' -not -name '*.hs' -delete
