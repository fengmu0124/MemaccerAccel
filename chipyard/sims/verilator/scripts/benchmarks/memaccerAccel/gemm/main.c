#include "main.h"
// way 0:rocket, 1:cache+accel, 2:dma+accel, 3:dma+spm+accel
void gemm(int way){
    // way 0:rocket, 1:cache+accel, 2:dma+accel, 3:dma+spm+accel
    uint64 cycles = 0;
    initArray(A, NM);	
	initArray(B, NM);
    if(way == 0){
        cycles = gemm_rocket(SIZE, A, B, C);
    }else{
        cycles = gemm_accel(3-way, SIZE, A, B, C);
    }
    printfVec(&C[0], 64, "C[0][*] value:");
	printf("Pass: %s, cost = %lld\n", (way==0 ? "rocket" : (way==1 ? "accelerator($)" : (way==2 ? "accelerator(dma)" : "accelerator(spm)"))), cycles);
}

int main(void){			
	gemm(3);
	return 0;	
}