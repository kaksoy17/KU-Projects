CC=gcc
CFLAGS=-I.

%.o: %.c 
	$(CC) -c -o $@ $< $(CFLAGS)

main: part3.o
	$(CC) -pthread -o part3 part3.o

pthread_sleep: pthread_sleep.o
	$(CC) -pthread -o pthread_sleep pthread_sleep.o

clean:
	rm *.o main
