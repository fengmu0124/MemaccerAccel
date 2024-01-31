#include "main.h"
// way 0:rocket, 1:cache+accel, 2:dma+accel, 3:dma+spm+accel

void gemm_opt(int way){
  // way 0:rocket, 1:cache+accel, 2:dma+accel, 3:dma+spm+accel
  uint64 start = 0, cycles = 0;
  initArray1D(A, NM);
  initArray1D(B, NM);
  if(way == 0){
    start = rdcycle();
    gemm_opt_rocket(SIZE, A, B, C);
    cycles = rdcycle() - start;
  }else{
    start = rdcycle();
    gemm_opt_accel(3-way, SIZE, A, B, C);
    cycles = rdcycle()  - 8*genConfig_Count - start;
  }
  printfVec(C, 64, "C[0-63] value:");
  printf("Pass: %s, cost = %lld\n", (way==0 ? "rocket" : (way==1 ? "accelerator($)" : (way==2 ? "accelerator(dma)" : "accelerator(spm)"))), cycles);
}

int main(void){		
	gemm_opt(3);
	return 0;	
}