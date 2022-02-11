CC=gcc
CFLAGS=-I.

%.o: %.c 
	$(CC) -c -o $@ $< $(CFLAGS)

main: part2.o
	$(CC) -pthread -o part2 part2.o

pthread_sleep: pthread_sleep.o
	$(CC) -pthread -o pthread_sleep pthread_sleep.o

clean:
	rm *.o main
