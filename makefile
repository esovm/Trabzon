
CC=gcc
CFLAGS=-Os -march=native -o

.PHONY: default all clean
default: all
all: trabzon
clean:
	rm -f trabzon
trabzon: trabzon.c
	$(CC) $(CFLAGS) $@ $^
