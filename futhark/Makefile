# SETUP
BFLGS = --backend=opencl
BDIR  = bench/
TDIR  = test/
FORM  = | awk -f form-tool.awk -

# TESTS

.PHONY: test

test: test-gmp-validation-compile test-gmp-validation-run test-fut-validation test-fut-properties

test-gmp-validation-compile:
	@echo "## COMPILING CPU GMP-VALIDATION TESTS"
	futhark c --library $(TDIR)test-gmp-validation-lib.fut
	gcc $(TDIR)test-gmp-validation.c -o $(TDIR)test-gmp-validation-cpu $(TDIR)test-gmp-validation-lib.c -lgmp
	@echo "## COMPILING GPU GMP-VALIDATION TESTS"
	futhark opencl --library $(TDIR)test-gmp-validation-lib.fut
	gcc $(TDIR)test-gmp-validation.c -o $(TDIR)test-gmp-validation-gpu $(TDIR)test-gmp-validation-lib.c -lgmp -lOpenCL -lm

test-gmp-validation-run:
	@echo "## RUNNING CPU GMP-VALIDATION TESTS"
	./$(TDIR)test-gmp-validation-cpu
	@echo "## RUNNING GPU GMP-VALIDATION TESTS"
	./$(TDIR)test-gmp-validation-gpu

test-fut-validation:
	@echo "## RUNNING CPU FUT-VALIDATION TESTS"
	futhark test --backend=c $(TDIR)test-add.fut $(TDIR)test-mul.fut
	@echo "## RUNNING GPU FUT-VALIDATION TESTS"
	futhark test --backend=c $(TDIR)test-add.fut $(TDIR)test-mul.fut

test-fut-properties:
	@echo "## RUNNING CPU FUT-PROPERTIES TESTS"
	futhark test --backend=c $(TDIR)test-add.fut $(TDIR)test-mul.fut
	@echo "## RUNNING GPU FUT-PROPERTIES TESTS"
	futhark test --backend=c $(TDIR)test-add.fut $(TDIR)test-mul.fut

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
	find . -not -name '.' -not -name 'bench' -not -name 'test' -not -name '*.fut' -not -name '*.awk' -not -name '.gitignore' -not -name 'Makefile' -delete
