#ifndef _common_h
#define _common_h

void initArray1D(TYPE *arr, int size){
	// 根据lgElem，每个元素值按地址顺序递增	
	for(int i = 0; i<size; ++i){
		arr[i] = i;
	}
}

// void initArray2D(TYPE **arr, int N, int M){
// 	for(int i=0; i<N; ++i){
// 		for(int j=0; j<M; ++j){
// 			arr[i][j] = i * M + j;
// 		}
// 	}
// }

void printfVec(TYPE *arr, int size, char* msg){
	printf("%s\n", msg);
	for(int i=0; i<size; ++i){
		printf("%lld(%d)\t", arr[i], i);
		if((i+1) % 8 == 0){
			printf("\n");
		}
	}
	printf("\n");
}

#endif