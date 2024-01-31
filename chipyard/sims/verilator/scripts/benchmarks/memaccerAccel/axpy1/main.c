#include "main.h"
// way 0:rocket, 1:cache+accel, 2:dma+accel, 3:dma+spm+accel
extern void axpy(int way, int N, int a, TYPE X[], TYPE Y[]);


int main(void){		
	for(int i=0; i<4; i++){
		axpy(i, MAXSIZE, 2, A, B);
	}	
	return 0;
	
}
