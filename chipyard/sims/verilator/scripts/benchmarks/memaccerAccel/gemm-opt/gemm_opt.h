#ifndef  _gemm_opt_h
#define  _gemm_opt_h

void gemm_opt_accel(int way, int size, TYPE A[], TYPE B[], TYPE C[]){
  int i=0, j=0, k=0;
  for(i=0; i<size; ++i){
    for(k=0; k<size; ++k){
      int a = A[i*size + k]; 
      for(j=0; j<size/NPE; ++j){
        load(&B[k*size + j*NPE], 1, NPE, 0, way);
        load(&C[i*size + j*NPE], 1, NPE, 1, way);
        compute(0, a, 1);
        store(&C[i*size + j*NPE], 1, NPE, -1, way);
      }
    }
  }
  flush(0);
}

void gemm_opt_rocket(int size, TYPE A[], TYPE B[], TYPE C[]) {
  int i=0, j=0, k=0;
  for(i=0; i<size; ++i){
    for(k=0; k<size; ++k){
      for(j=0; j<size; ++j){
        C[i*size + j] += A[i*size + k]*B[k*size + j];
      }
    }
  }
}
#endif