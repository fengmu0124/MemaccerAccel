#include "main.h"
// way 0:rocket, 1:cache+accel, 2:dma+accel, 3:dma+spm+accel
void covariance(int way){
  // way 0:rocket, 1:cache+accel, 2:dma+accel, 3:dma+spm+accel
  uint64 start = 0, cycles = 0;
  initArray1D(DATA, NM);
  genConfig_Count = 0;
  if(way == 0){
    start = rdcycle();
    covariance_rocket(SIZE, SIZE, SIZE, DATA, SYMMAT, MEAN);
    cycles = rdcycle() - start;
  }else{
    cycles = covariance_accel(3-way, SIZE, SIZE, SIZE, DATA, SYMMAT, MEAN);
    cycles = cycles  - 8*genConfig_Count - start;
  }
  printfVec(SYMMAT, 64, "SYMMAT[0-63] value:");
  printf("Pass: %s, cost = %lld\n", (way==0 ? "rocket" : (way==1 ? "accelerator($)" : (way==2 ? "accelerator(dma)" : "accelerator(spm)"))), cycles);
}

int main(void){		
	printf("addr: data=%llx, symmat=%llx, mean=%llx\n", DATA, SYMMAT, MEAN);
	covariance(3);
	return 0;
	
}