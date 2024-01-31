#include "main.h"

void kmean(int way){
    // way 0:rocket, 1:cache+accel, 2:dma+accel, 3:dma+spm+accel
    uint64 start = 0, cycles = 0, iter=10;
    if(way == 0){
        start = rdcycle();
        kmean_rocket(iter, c, cc, labels, data);
        cycles = rdcycle() - start;
    }else{        
        cycles = kmean_accel(3-way, iter, c, cc, labels, data);
        cycles = cycles - 8*genConfig_Count;
        genConfig_Count = 0;
    }
    printfVec(&labels[0], 32, "labels[0-32] value:");
	printf("Pass: %s, cost = %lld\n", (way==0 ? "rocket" : (way==1 ? "accelerator($)" : (way==2 ? "accelerator(dma)" : "accelerator(spm)"))), cycles);
}

int main(void){		
	kmean(3);
	return 0;
	
}