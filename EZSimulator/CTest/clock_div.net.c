#include <stdio.h>

int main() {
	/*char _screenW = readByte();
	char _screenH = readByte();
	char* _rom = readRom();*/
	char a = 0;
	char o = 0;

	for(int i = 0; i < 1000000000; i ++) {
		o = !a;
		a = o;
	}
}