#ifndef _axpy_h
#define _axpy_h

uint64 axpy_accel(int way, int N, int a, TYPE X[], TYPE Y[]){
    uint64 cycles = 0, start = 0, res=0;
    start = rdcycle();
    for(int i=0; i<N/NPE; ++i){
        cycles += rdcycle() - start;
        cycles += dload(&X[i*NPE], 1, NPE, 0, way);
        cycles += dload(&Y[i*NPE], 1, NPE, 1, way);
        cycles += dcompute(0, &res, a, 1);
        cycles += dstore(&Y[i*NPE], 1, NPE, -1, way);        
        start = rdcycle();
    }
    cycles += rdcycle() - start;
    cycles += dflush(0);
    return cycles;
}


uint64 axpy_rocket(int N, int a, TYPE X[], TYPE Y[]){
    uint64 start = 0;
    start = rdcycle();
    for(int i = 0; i<N; ++i){
        Y[i] = a * X[i] + Y[i];
    }
    return rdcycle() - start;
}

void axpy(int way, int N, int a, TYPE X[], TYPE Y[]){
    // way 0:rocket, 1:cache+accel, 2:dma+accel, 3:dma+spm+accel
    uint64 cycles = 0;
    initArray(&A[0], MAXSIZE);	
	initArray(&B[0], MAXSIZE);
    if(way == 0){
        cycles = axpy_rocket(N, a, X, Y);
    }else{
        cycles = axpy_accel(3-way, N, a, X, Y);
    }
    printfVec(&B[0], 64, "B[0-63] value:");
	printf("Pass: %s, cost = %lld\n", (way==0 ? "rocket" : (way==1 ? "accelerator($)" : (way==2 ? "accelerator(dma)" : "accelerator(spm)"))), cycles);
}

#endif