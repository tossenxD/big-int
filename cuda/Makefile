CXX      = nvcc -O3 -arch=sm_70
SRC      = main.cu
EXE      = big-int-tests
BIG      = 134217728
SML      = 4194304

default: compile run

compile:
	$(CXX) $(SRC) -o $(EXE) -lgmp

run:
	./$(EXE) $(BIG)

clean:
	rm -f $(EXE)
