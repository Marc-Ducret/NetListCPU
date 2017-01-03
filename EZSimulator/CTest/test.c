#include "stdio.h"

int main() {
	char o;
	char x;
	char y[3];
	
	while(1) {
		o = x;
		y[1] = !o;
		x = y[1];
		printf("lol x=%d, o=%d\n", x, o);
	}
}