#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

char* readRom() {
	char sizePow;
	read(0, &sizePow, 1);
	int size = 1 << sizePow;
	printf("size=%d\n", size);
	char* rom = (char *) malloc(size);
	read(0, rom, size);
	return rom;
}

char readByte() {
	printf("waiting to read....");
	char buf;
	read(0, &buf, 1);
	printf("OK\n");
	return buf;
}

void writeExit() {
	char buf = 0;
	write(0, &buf, 1);
	fflush(stdout);
}

void writeRedraw() {
	char buf = 2;
	write(0, &buf, 1);
	fflush(stdout);
}

void writeChar(char x, char y, char c) {
	char buf = 1;
	write(0, &buf, 1);
	write(0, &x, 1);
	write(0, &y, 1);
	write(0, &c, 1);
}

void rom(char * dest, char * _rom, char wordSize, int _addr) {
	char realSize = (wordSize+7) / 8;
	for(int i = 0; i < wordSize; i ++) {
		char mask = 1 << (i % 8);
		dest[wordSize - i - 1] = !!(_rom[_addr*realSize + i / 8] & mask);
	}
}






