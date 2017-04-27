target: decide

CC=mpicc
CFLAGS= -g -O3 -std=c99

mainMPIpur.o: mainMPIpur.c projet.h
aux.o: aux.c projet.h

decide: mainMPIpur.o aux.o
	$(CC) $(CFLAGS) $^ -o $@

.PHONY: clean exec

clean:
	rm -f *.o decide

exec:
#	mpirun -n 4 -hostfile hostfile --bynode ./decide "7K//k1P/7p w" 
	mpirun -n 4 -hostfile hostfile --bynode ./decide "4k//4K/4P w"
#	mpirun -n 4 -hostfile hostfile --bynode ./decide "/ppp//PPP//7k//7K w"
#	mpirun -n 4 -hostfile hostfile --bynode ./decide "K////2k3P/6P/6P/ w"
