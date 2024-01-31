#include "main.h"

// common.h
extern void initArray(TYPE *arr, int size);
extern void printfVec(TYPE *arr, int size, char* msg);
// lib.h
extern uint64 load(uint64 baseAddr, uint64 stride, uint64 elems, uint64 loadSrc, uint64 way);
extern uint64 compute(uint64 op, uint64* res, uint64 scalaA, uint64 scalaB);
extern uint64 store(uint64 baseAddr, uint64 stride, uint64 elems, uint64 vmask, uint64 way);
extern uint64 flush(uint64 op);
void testGenMask(){
	uint64 A[64] = {0};
	uint64 B[64] = {0};
	uint64 one = 1;
	uint64 trueMask = 0, calMask = 0; 
	
	for(int i=0; i<64;++i){
		A[i] = i;
		if(i%2==0){
			B[i] = i + 1;
		}
		if(A[i]<B[i]){
			trueMask = trueMask | (one<<i);
		}
		//printf("A=%lld, B=%lld, trueMask=%llx\n", A[i], B[i], one, trueMask);
	}
	
	dload(A, 1, 64, 0, 3);
	dload(B, 1, 64, 1, 3);
	calMask = dcompute(5, 1, 1);
	printf("trueMask=%llx, calMask=%llx\n", trueMask, calMask);
}
int main(void){		
	initArray(&A[0], MAXSIZE);
	dload(A, 1, 64, 0, 0);
	dload(A+2, 1, 64, 1, 0);
	uint64 a = 0;
	dcompute(0, &a, 1, 1);
	dstore(B, 1, 64, -1, 0);
	dflush(0);
	printfVec(B, 64, "B[0-63] result: ");
	return 0;
	
}