CXX = nvcc -O3 -arch=sm_70

SRC_CGBN = cgbn-perf.cu
EXE_CGBN = big-int-cgbn

# This file is heavily based on:

#   ``GPU Implementations for Midsize Integer Addition and Multiplication'',
#   by Cosmin E. Oancea and Stephen M. Watt, 2024. [2]
#   paper published:       https://arxiv.org/abs/2405.14642
#   source code published: https://github.com/coancea/midint-arithmetic

default: run-1024

all: run-1024 run-512 run-256 run-128 run-64 run-32 run-16
#all: run-8192 run-4096 run-2048 run-1024 run-512 run-256 run-128 run-64 run-32 run-16

run-8192:
	$(CXX) -ICGBN -DNUM_BITS=262144 -DTHD_PER_INST=32 $(SRC_CGBN) -o $(EXE_CGBN) -lgmp
	./$(EXE_CGBN) 16384
	@echo ""

run-4096:
	$(CXX) -ICGBN -DNUM_BITS=131072 -DTHD_PER_INST=32 $(SRC_CGBN) -o $(EXE_CGBN) -lgmp
	./$(EXE_CGBN) 32768
	@echo ""

run-2048:
	$(CXX) -ICGBN -DNUM_BITS=65536 -DTHD_PER_INST=32 $(SRC_CGBN) -o $(EXE_CGBN) -lgmp
	./$(EXE_CGBN) 65536
	@echo ""

run-1024:
	$(CXX) -ICGBN -DNUM_BITS=32768 -DTHD_PER_INST=32 $(SRC_CGBN) -o $(EXE_CGBN) -lgmp
	./$(EXE_CGBN) 131072
	@echo ""

run-512:
	$(CXX) -ICGBN -DNUM_BITS=16384 -DTHD_PER_INST=32 $(SRC_CGBN) -o $(EXE_CGBN) -lgmp
	./$(EXE_CGBN) 262144
	@echo ""

run-256:
	$(CXX) -ICGBN -DNUM_BITS=8192 -DTHD_PER_INST=32 $(SRC_CGBN) -o $(EXE_CGBN) -lgmp
	./$(EXE_CGBN) 524288
	@echo ""

run-128:
	$(CXX) -ICGBN -DNUM_BITS=4096 -DTHD_PER_INST=16 $(SRC_CGBN) -o $(EXE_CGBN) -lgmp
	./$(EXE_CGBN) 1048576
	@echo ""

run-64:
	$(CXX) -ICGBN -DNUM_BITS=2048 -DTHD_PER_INST=8 $(SRC_CGBN) -o $(EXE_CGBN) -lgmp
	./$(EXE_CGBN) 2097152
	@echo ""

run-32:
	$(CXX) -ICGBN -DNUM_BITS=1024 -DTHD_PER_INST=4 $(SRC_CGBN) -o $(EXE_CGBN) -lgmp
	./$(EXE_CGBN) 4194304
	@echo ""

run-16:
	$(CXX) -ICGBN -DNUM_BITS=512  -DTHD_PER_INST=2 $(SRC_CGBN) -o $(EXE_CGBN) -lgmp
	./$(EXE_CGBN) 8388608
	@echo ""

clean:
	rm -f $(EXE_CGBN) $(EXE_OUR)

cgbn-prep:
# Download / update CGBN
	git clone git@github.com:NVlabs/CGBN.git "CGBN" || true
# Patch outdated Makefile
	sed -i 's/master/main/g' CGBN/Makefile
	sed -i 's/ -std=c++11 / -std=c++14 /' CGBN/unit_tests/Makefile
