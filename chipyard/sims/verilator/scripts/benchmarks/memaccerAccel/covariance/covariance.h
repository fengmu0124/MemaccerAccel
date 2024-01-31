#ifndef _covariance_h
#define _covariance_h

uint64 covariance_accel(int way, int M, int N, uint64 float_n, TYPE *data, TYPE *symmat, TYPE *mean){
    int i, j, j1, j2;
    uint64 sum=0, rd=0;
    uint64 start = 0, cycles=0;
    start = rdcycle();
    //  part 1
    for(j=0; j<M; ++j){
        sum = 0;
        for(i=0; i<N/NPE; ++i){
          load(&data[NPE*i*M+j], M, NPE, 0, way);
          sum += compute(4, 1, 1);
        }
        //asm volatile("fence");// rocc / rocket 端同时访问可能回出问题
        mean[j] = sum / float_n;
        //asm volatile("fence":::"memory");
    }
    
    // part 2
    for(i=0; i<N; ++i){
        for(j=0; j<M/NPE; ++j){
          cycles += rdcycle() - start;  // todo 添加了这三条语句就才能够执行通过
          printf("finnsh %d\n", j);
          start = rdcycle();
          load(&data[i*M + j*NPE], 1, NPE, 0, way);
          load(&mean[j*NPE], 1, NPE, 1, way);            
          compute(1, 1, 1);
          store(&data[i*M + j*NPE], 1, NPE, -1, way);
        }
    }

    for(j1=0; j1<M; ++j1){
        for(j2=j1; j2<M; ++j2){
            sum = 0;
            for(i=0; i<N/NPE; ++i){
              load(&data[NPE*i*M + j1], M, NPE, 0, way);
              load(&data[NPE*i*M + j2], M, NPE, 1, way);
              sum += compute(4, 1, 1);            
             
            }
            symmat[j2*M+j1] = sum;
            symmat[j1*M+j2] = sum;
        }
    }
    flush(0);
    cycles += rdcycle() - start;
    return cycles;
}

void covariance_rocket(int M, int N, int float_n, TYPE *data, TYPE *symmat, TYPE *mean) {
    int i, j, j1, j2;
    /* Determine mean of column vectors of input data matrix */
    for (j = 0; j < M; j++) {
      mean[j] = 0.0;
      for (i = 0; i < N; i++)
        mean[j] += data[i*M + j];
      mean[j] /= float_n;
    }

    /* Center the column vectors. */
    for (i = 0; i < N; i++)
      for (j = 0; j < M; j++)
        data[i*M + j] -= mean[j];

    /* Calculate the m * m covariance matrix. */
    for (j1 = 0; j1 < M; j1++)
      for (j2 = j1; j2 < M; j2++) {
        symmat[j1*M + j2] = 0;
        for (i = 0; i < N; i++)
          symmat[j1*M+j2] += data[i*M + j1] * data[i*M+j2];
        symmat[j2*M + j1] = symmat[j1*M + j2];
      }
}

#endif