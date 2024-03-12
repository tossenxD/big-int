CXX      = nvcc -O3 -arch=sm_70
SRC      = main.cu
EXE      = big-int-tests
BIG      = 134217728
SML      = 4194304

default: compile run-big

small: compile run-small

compile:
	$(CXX) $(SRC) -o $(EXE) -lgmp

run-big:
	./$(EXE) $(BIG)

run-small:
	./$(EXE) $(SML)

clean:
	rm -f $(EXE)
