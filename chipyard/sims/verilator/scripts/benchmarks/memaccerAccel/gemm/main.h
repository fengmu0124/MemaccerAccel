#ifndef _main_h
#define _main_h

#include<stdio.h>
#include "rocc.h"
#include "encoding.h"

#define LGELEM 2    // 同时改变
typedef unsigned int TYPE;

// #define LGELEM 3    // 同时改变
// typedef unsigned long long TYPE;

typedef unsigned long long uint64;

#define NPE 64
// #define SIZE 128
// #define NM 16384 
#define SIZE 64
#define NM 4096 

TYPE A[NM] = {0};
TYPE B[NM] = {0};
TYPE C[NM] = {0};

#include "common.h"
#include "lib.h"
#include "gemm.h"

#endif
