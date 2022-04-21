#include <stdlib.h>
#include <stdio.h>
extern void p(long*,long*,long*);
extern void f(long*,long*,long*);
extern void g(long*,long*,long*);

int main(){
	long* A = (long*)calloc(100, sizeof(long));
	long* B = (long*)calloc(100, sizeof(long));
	long* C1 = (long*)calloc(100, sizeof(long));
	long* C2 = (long*)calloc(100, sizeof(long));
	long* C3 = (long*)calloc(100, sizeof(long));
	
	for(int i = 0; i < 100; i++){
		A[i] = i % 10;
		B[i] = i % 7;
	}
	
	p(A,B,C1);
	f(A,B,C2);
	g(A,B,C3);
	
	for(int i = 99; i >= 0; i--)
	{
		printf("%ld : %ld : %ld\n", C1[i], C2[i], C3[i]);
	}
}
