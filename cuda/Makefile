CXX      = nvcc -O3 -arch=sm_70
SRC      = main.cu
EXE      = big-int-tests

default: compile run

compile:
	$(CXX) $(SRC) -o $(EXE) -lgmp

run:
	./$(EXE) 8388608

clean:
	rm -f $(EXE)
