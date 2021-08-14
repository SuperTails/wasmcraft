/*void store_across(short *p, int val) {
	int *lo = (int*)(p - 1);
	int *hi = (int*)(p + 1);

	int l = *lo;
	*lo = (l & 0xFFFF0000) | (val & 0xFFFF);
	*hi = (l & 0x0000FFFF) | ((val << 16));
}*/

/*int times_three(int a) {
	return a * 3;
}
*/

int do_stuff(int a, int b, int c) {
	return -b + (b*b - 4 * a * c) / 2 * a;
}

int _start() {
	int total = 0;

	for (int i = 0; i < 10; ++i) {
		total += i;
	}

	//return (b - 4 * c) / 2;

	return total;

	//return times_three(2);
	//return do_stuff(3, 5, 7);
	//return 42 / 6;
}