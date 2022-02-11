CC=gcc
CFLAGS=-I.

%.o: %.c 
	$(CC) -c -o $@ $< $(CFLAGS)

main: part1.o
	$(CC) -pthread -o part1 part1.o

pthread_sleep: pthread_sleep.o
	$(CC) -pthread -o pthread_sleep pthread_sleep.o

clean:
	rm *.o main
