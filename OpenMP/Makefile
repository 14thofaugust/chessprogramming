target: decide

CFLAGS=-g -O3 -std=c99 -fopenmp

mainOpenMP.o: mainOpenMP.c projet.h
aux.o: aux.c projet.h


decide: mainOpenMP.o aux.o
	$(CC) $(CFLAGS) $^ -o $@

.PHONY: clean exec

clean:
	rm -f *.o decide

exec:
	export OMP_NUM_THREADS=8
        export OMP_SCHEDULE="dynamic"
        export OMP_NESTED=FALSE
        ./decide "4k//4K/4P w"