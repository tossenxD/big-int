CXX      = nvcc -O3 -arch=sm_70
SRC      = add.cu
EXE      = big-num-add
HELPERS  = helper.h

default: compile run

compile:
	$(CXX) $(SRC) -o $(EXE) -lgmp

run:
	./$(EXE) 2 3

clean:
	rm -f $(EXE)
