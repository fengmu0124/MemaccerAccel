#ifndef _common_h
#define _common_h

void initArray(TYPE *arr, int size){
	// 根据lgElem，每个元素值按地址顺序递增	
	for(int i = 0; i<size && i<MAXSIZE; ++i){
		arr[i] = i;
	}
}

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