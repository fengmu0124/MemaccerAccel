#ifndef _kmean_h
#define _kmean_h

uint64 kmean_accel(int way, int iter, TYPE c[], TYPE cc[], TYPE labels[], TYPE data[]){
    int h, i, j, rd=0;
    TYPE counts[K] = {0};
    uint64 start = 0, cycles = 0;       
    start = rdcycle();
    // 初始化质心
    for(i=0; i<K; ++i){
        for(j=0; j<M/NPE; ++j){
            load(&data[i*M + j*NPE], 1, NPE, 2, way);
            store(&c[i*M + j*NPE], 1, NPE, -1, way);
        }
    }

    // 核心循环, 迭代100次, 不考虑收敛
    while(iter--){
        for(i=0; i<K; ++i){
            counts[i] = 0;
            for(j=0; j<M/NPE; ++j){
                compute(0, 0, 0); // 生成0向量
                store(&cc[i*M+j*NPE], 1, NPE, -1, way);
            }
        }

        for(h=0; h<N; ++h){
            float min_distance = FLT_MAX;
            for(i=0; i<K; ++i){
                float distance = 0;
                for(j=0; j<M/NPE; ++j){
                    load(&data[h*M + j*NPE], 1, NPE, 0, way);
                    load(&c[i*M + j*NPE], 1, NPE, 1, way);
                    compute(4, 1, 1);
                }
                if(rd < min_distance){
                    labels[h] = i;
                    min_distance = distance;
                }
            }
            for(j = 0; j<M/NPE; ++j){                
                load(&data[h*M + j*NPE], 1, NPE, 0, way);//todo 传参错误,需加上printf
                cycles += rdcycle() - start;
                printf("%llx\n", &data[h*M + j*NPE]);
                start = rdcycle();
                load(&cc[i*M + j*NPE], 1, NPE, 1, way);
                compute(0, 1, 1);
                store(&cc[i*M + j*NPE], 1, NPE, -1, way);
            }
            counts[labels[h]]++;
        }

        for(i=0; i<K; ++i){
            for(j=0; j<M/NPE; ++j){
                load(&cc[i*M + j*NPE], 1, NPE, 0, way);
                compute(2, 1, 2);
                store(&cc[i*M + j*NPE], 1, NPE, -1, way);
            }
        }
    }
    flush(0);
    cycles += rdcycle() - start;
    return cycles;
}


void kmean_rocket(int iter, TYPE c[], TYPE cc[], TYPE labels[], TYPE data[]){    
    int h, i, j;
    TYPE counts[K]    = {0};
    //float old_error, error = FLT_MAX;
    
    // 初始化质心
    for(i=0; i<K; i++){
        for(j=0; j < M; ++j){
            c[i*M + j] = data[i*M + j];
        }
    }

    // 核心循环
    while(iter--){
        // counts, c1清0
        for(i = 0; i < K; ++i){
            counts[i] = 0;
            for(j = 0; j < M; ++j){
                cc[i*M + j] = 0;
            }
        }
        
        for(h = 0; h < N; ++h){
            float min_distance = FLT_MAX;
            for(i = 0; i < K; ++i){
                float distance = 0;
                for(j = 0; j < M;  ++j){
                    int tmp = data[h*M+j] - c[i*M+j];
                    distance += tmp * tmp;
                }
                if(distance < min_distance){
                    labels[h] = i;
                    min_distance = distance;
                }
            }

            for(j = 0; j<M; ++j){
                cc[labels[h]*M + j] += data[h*M+j];
            }

            counts[labels[h]]++;
        }
        for(i=0; i<K; i++){
            for(j=0; j<M; j++){
                c[i*M + j] = counts[i] ? cc[i*M + j] / counts[i] : cc[i*M + j];
            }
        }
    }
}


#endif