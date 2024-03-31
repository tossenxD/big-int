EXES  = bench-one-add bench-ten-add bench-one-mul bench-ten-mul test
TFLGS = --backend=opencl 
BFLGS = --backend=opencl 

test:
	futhark test $(TFLGS) test.fut

add: one-add ten-add

one-add:
	futhark bench $(BFLGS) bench-one-add.fut

ten-add:
	futhark bench $(BFLGS) bench-ten-add.fut

mul: one-mul ten-mul

one-mul:
	futhark bench $(BFLGS) bench-one-mul.fut

ten-mul:
	futhark bench $(BFLGS) bench-ten-mul.fut

clean:
	rm -rf $(EXES) *.c *.expected *.actual data/
