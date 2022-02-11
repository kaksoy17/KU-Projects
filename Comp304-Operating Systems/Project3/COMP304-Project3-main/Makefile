CC=gcc
CFLAGS=-I.

all: part1 part2

%.o: %.c 
	$(CC) -c -o $@ $< $(CFLAGS)

part1: part1.o
	$(CC) -o part1 part1.o

part2: part2.o
	$(CC) -o part2 part2.o

clean:
	rm *.o part1 part2