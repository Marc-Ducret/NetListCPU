#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <time.h>

int threadRun;
char *_ram_ref;

void init(char *_ram) {
	freopen(NULL, "rb", stdin);
	_ram_ref = _ram;
}

char* readRom() {
	char sizePow;
	read(0, &sizePow, 1);
	int size = 1 << sizePow;
	fprintf(stderr, "size=%d\n", size);
	fflush(stderr);
	unsigned char* rom = (unsigned char *) malloc(size);
	read(0, rom, size);
	return rom;
}

char readByte() {
	char buf;
	read(0, &buf, 1);
	return buf;
}

void writeExit() {
	fprintf(stderr, "exit!\n");
	fflush(stderr);
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
		val |= data[wordSize-i-1] << i;
	return val;
}

void rom(char * dest, char * _rom, char wordSize, int _addr) {
	char realSize = (wordSize+7) / 8;
	for(int i = 0; i < wordSize; i ++) {
		char mask = 0x80 >> (i % 8);
		dest[i] = !!(_rom[_addr*realSize + i / 8] & mask);
	}
}

void printBitVector(char * vec, int size, char * name) {
	fprintf(stderr, "%s:\t%x\n", name, toInt(vec, size));
}

void *keyThread(void *arg) {
	while(threadRun) {
		char action = readByte();
		char key = readByte();
		if(action == 0x10) {
			fprintf(stderr, "press: %d\n", key);
			_ram_ref[0x10005 + key] = 1;
		} else if(action == 0x20) {
			fprintf(stderr, "reals: %d\n", key);
			_ram_ref[0x10005 + key] = 0;
		} else {
			fprintf(stderr, "Unknown code: %d\n", action);
		}
	}
	pthread_exit(NULL);
}

void runKeyThread() {
	threadRun = 1;
	pthread_t thread;
	if (pthread_create(&thread, NULL, keyThread, NULL)) {
		fprintf(stderr, "Cannot create key thread\n");
    } else {
		fprintf(stderr, "key thread started\n");
	}
}

void exitKeyThread() {
	threadRun = 0;
}