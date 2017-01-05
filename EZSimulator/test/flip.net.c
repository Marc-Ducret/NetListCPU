#include "stdio.h"

int main() {
	char a;
	char b;
	char o;

	while(1) {
		b = !a;
		o = b;
		a = o;
	}
}