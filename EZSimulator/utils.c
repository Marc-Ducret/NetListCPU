#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

char* readRom() {
	char sizePow;
	read(0, &sizePow, 1);
	int size = 1 << sizePow;
	char* rom = (char *) malloc(size);
	read(0, rom, size);
	return rom;
}

char readByte() {
	char buf;
	read(0, &buf, 1);
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