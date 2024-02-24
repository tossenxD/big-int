CXX      = nvcc -O3 -arch=sm_70
SRC      = main.cu
EXE      = big-int-tests

default: compile run

compile:
	$(CXX) $(SRC) -o $(EXE) -lgmp

run:
	./$(EXE) 2048

clean:
	rm -f $(EXE)
