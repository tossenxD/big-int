TFLGS = --backend=opencl 
BFLGS = --backend=opencl
BDIR  = bench/
TDIR  = test/
FORM  = | awk -f form-tool.awk -



.PHONY: test test-add test-mul

test:
	futhark test $(TFLGS) $(TDIR)test-add.fut $(TDIR)test-mul.fut

test-add:
	futhark test $(TFLGS) $(TDIR)test-add.fut

test-mul:
	futhark test $(TFLGS) $(TDIR)test-mul.fut



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



clean:
	find . -not -name '.' -not -name 'bench' -not -name 'test' -not -name '*.fut' -not -name '*.awk' -not -name '.gitignore' -not -name 'Makefile' -delete
