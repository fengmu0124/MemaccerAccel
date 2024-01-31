#ifndef _floyd_warshall_h
#define _floyd_warshall_h



void initPath(int size, TYPE path[]){
    int i, j;
    for(i=0; i<size; i++){
        for(j=0; j<size; j++){
            path[i*size + j] = i*j%7 + 1;
            if ((i+j)%13 == 0 || (i+j)%7==0 || (i+j)%11 == 0){
                path[i*size + j] = 999;
            }         
        }
    }
}

void floyd_warshall_rocket(int size, TYPE path[]){
    int i, j, k;
    TYPE pk[SIZE] = {0};
    for(k=0; k<size; ++k){
        for(i=0; i<size; ++i){       
            
            TYPE tmp1 = path[i*size + k] < path[i*size+k] + path[k*size+k] ? path[i*size+j] : path[i*size+k] + path[k*size+k];
            TYPE tmp2 = path[i*size + k];
            for(j=0; j<size; ++j){
                pk[j] = (j>k ? tmp1 : tmp2) + path[k*size + j];            
            }
            
            for(j=0; j<size; ++j){               
                path[i*size+j] = path[i*size+j] < pk[j] + path[k*size+j] ? path[i*size+j] : pk[j] + path[k*size+j];                              
            }
        }
    }
}

void floyd_warshall_accel(int way, int size, TYPE path[]){
    int i, j, k;
    uint64 rd = 0;

    TYPE pk[SIZE] = {0};
   
    
    for(k=0; k<size; ++k){
        for(i=0; i<size; ++i){
            TYPE tmp1 = path[i*size + k] < path[i*size+k] + path[k*size+k] ? path[i*size+j] : path[i*size+k] + path[k*size+k];
            TYPE tmp2 = path[i*size+k];
            
            for(j=0; j<size/NPE; ++j){
                pk[j] = (j>k ? tmp1 : tmp2) + path[k*size+j];            
            }

            for(j=0; j<size/NPE; ++j){
                load(&pk[i*size + j*NPE], 1, NPE, 0, way);
                load(&path[i*size + j*NPE], 1, NPE, 1, way);                
                rd = compute(5, 1, 1);  // src0 < src1 返回 1
                load(&pk[i*size + j*NPE], 1, NPE, 2, way);
                store(&path[i*size + j*NPE], 1, NPE, rd, way);
            }
            flush(0);
        }
    }
}


#endif