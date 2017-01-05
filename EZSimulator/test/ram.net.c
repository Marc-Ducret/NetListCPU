#include "stdio.h"

int main() {
	char _l_10_22 = 0;
	char _l_10_35 = 0;
	char _l_10_48 = 0;
	char _l_10_61 = 0;
	char _l_11_21 = 0;
	char _l_11_34 = 0;
	char _l_11_47 = 0;
	char _l_11_60 = 0;
	char _l_12_20[3] = {0};
	char _l_12_33[2] = {0};
	char _l_12_46[1] = {0};
	char _l_13_19[3] = {0};
	char _l_13_32[2] = {0};
	char _l_13_45[1] = {0};
	char _l_14_18[3] = {0};
	char _l_14_31[2] = {0};
	char _l_14_44[1] = {0};
	char _l_16[4] = {0};
	char _l_9_23 = 0;
	char _l_9_36 = 0;
	char _l_9_49 = 0;
	char _l_9_62 = 0;
	char c[4] = {0};
	char o[4] = {0};
	char ra[2] = {0};
	char wa[2] = {0};
	char we = 0;
	char _ram[16] = {0};

	while(1) {
		ra[0] = 1;
		ra[1] = 0;
		we = 1;
		wa[0] = 1;
		wa[1] = 0;
		int _addr = 0, _pow = 1;
		for(int i = 0; i < 2; i++) {
			_addr += _pow * ra[i];
			_pow *= 2;
		}
		for(int i = 0; i < 4; i++) o[i] = _ram[_addr*4 + i];
		_l_9_23 = o[0];
		for(int i = 1; i <= 3; i ++) _l_12_20[i-1] = o[i];
		_l_9_36 = _l_12_20[0];
		for(int i = 1; i <= 2; i ++) _l_12_33[i-1] = _l_12_20[i];
		_l_9_49 = _l_12_33[0];
		for(int i = 1; i <= 1; i ++) _l_12_46[i-1] = _l_12_33[i];
		_l_9_62 = _l_12_46[0];
		_l_10_22 = c[0];
		_l_11_21 = _l_9_23 | _l_10_22;
		for(int i = 1; i <= 3; i ++) _l_13_19[i-1] = c[i];
		_l_10_35 = _l_13_19[0];
		_l_11_34 = _l_9_36 | _l_10_35;
		for(int i = 1; i <= 2; i ++) _l_13_32[i-1] = _l_13_19[i];
		_l_10_48 = _l_13_32[0];
		_l_11_47 = _l_9_49 | _l_10_48;
		for(int i = 1; i <= 1; i ++) _l_13_45[i-1] = _l_13_32[i];
		_l_10_61 = _l_13_45[0];
		_l_11_60 = _l_9_62 | _l_10_61;
		_l_14_44[0] = _l_11_60;
		for(int i = 0; i < 1; i++)
			_l_14_31[i] = _l_11_47;
		for(int i = 0; i < 1; i++)
			_l_14_31[i+1] = _l_14_44[i];
		for(int i = 0; i < 1; i++)
			_l_14_18[i] = _l_11_34;
		for(int i = 0; i < 2; i++)
			_l_14_18[i+1] = _l_14_31[i];
		for(int i = 0; i < 1; i++)
			_l_16[i] = _l_11_21;
		for(int i = 0; i < 3; i++)
			_l_16[i+1] = _l_14_18[i];
		_addr = 0; _pow = 1;
		for(int i = 0; i < 2; i++) {
			_addr += _pow * wa[i];
			_pow *= 2;
		}
		for(int i = 0; i < 4; i++) _ram[_addr*4 + i] = _l_16[i];
		printf("o = %d%d%d%d\n", o[0], o[1], o[2], o[3]);
		scanf("%d", c);
		scanf("%d", c+1);
		scanf("%d", c+2);
		scanf("%d", c+3);
		printf("c = %d%d%d%d\n", c[0], c[1], c[2], c[3]);
	}
}