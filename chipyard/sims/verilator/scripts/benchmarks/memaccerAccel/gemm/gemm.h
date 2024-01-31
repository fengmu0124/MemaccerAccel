#ifndef _gemm_h
#define _gemm_h


uint64 gemm_accel(int way, int size, TYPE A[], TYPE B[], TYPE C[]){
    uint64 cycles = 0, start = 0, sum=0, rd=0;
    start = rdcycle();
    
    for(int i=0; i<size; ++i){
        for(int j=0; j<size; ++j){
            sum = 0;
            for(int k=0; k<size/NPE; ++k){
                cycles += rdcycle() - start;
                cycles += load(&A[i*size + k*NPE], 1, NPE, 0, way);
                cycles += load(&B[j + NPE*k*size], size, NPE, 1, way);
                cycles += compute(4, &rd, 1, 1);
                start = rdcycle();
                sum += rd;
            }
            C[i*size + j] = sum;
        }
    }

    cycles += rdcycle() - start;
    return cycles;
}


uint64 gemm_rocket(int size, TYPE A[], TYPE B[], TYPE C[]){
    uint64 start = 0;
    start = rdcycle();
    for(int i=0; i<size; ++i){
        for(int j=0; j<size; ++j){
            int sum = 0;
            for(int k=0; k<size; ++k){
                sum += A[i*size + k] * B[j + k*size];
            }
            C[i*size + j] = sum;
        }
    }
    return rdcycle() - start;
}
#endif