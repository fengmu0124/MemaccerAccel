#include "main.h"

void floyd_warshall(int way){
  // way 0:rocket, 1:cache+accel, 2:dma+accel, 3:dma+spm+accel
  uint64 start = 0, cycles = 0;
  initPath(SIZE, PATH);
  if(way == 0){
    start = rdcycle();
    floyd_warshall_rocket(SIZE, PATH);
    cycles = rdcycle() - start;
  }else{
    start = rdcycle();
    floyd_warshall_accel(3-way, SIZE, PATH);
    cycles = rdcycle() - start - 8*genConfig_Count;
  }
  printfVec(PATH, 64, "PATH[0-63] value:");
  printf("Pass: %s, cost = %lld\n", (way==0 ? "rocket" : (way==1 ? "accelerator($)" : (way==2 ? "accelerator(dma)" : "accelerator(spm)"))), cycles);
}



int main(void){	
	floyd_warshall(1);
	return 0;
	
}