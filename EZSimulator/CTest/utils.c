#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

char* readRom() {
	char sizePow;
	read(0, &sizePow, 1);
	int size = 1 << sizePow;
	fprintf(stderr, "size=%d\n", size);
	char* rom = (char *) malloc(size);
	read(0, rom, size);
	fflush(stderr);
	return rom;
}

char readByte() {
	fprintf(stderr, "waiting to read....");
	char buf;
	read(0, &buf, 1);
	fprintf(stderr, "OK[%d]\n", buf);
	fflush(stderr);
	return buf;
}

void writeExit() {
	char buf = 0;
	write(1, &buf, 1);
	fflush(stdout);
}

void writeRedraw() {
	char buf = 2;
	write(1, &buf, 1);
	fflush(stdout);
}

void writeChar(char x, char y, char c) {
	char buf = 1;
	write(1, &buf, 1);
	write(1, &x, 1);
	write(1, &y, 1);
	write(1, &c, 1);
	fflush(stdout);
}

int toInt(char * data, char wordSize) {
	int val = 0;
	for(int i = 0; i < wordSize; i++)
		val |= data[i] << i;
	return val;
}

void rom(char * dest, char * _rom, char wordSize, int _addr) {
	char realSize = (wordSize+7) / 8;
	for(int i = 0; i < wordSize; i ++) {
		char mask = 1 << (i % 8);
		dest[i] = !!(_rom[_addr*realSize + i / 8] & mask);
	}
}






