#ifndef _main_h
#define _main_h

#include<stdio.h>
#include "rocc.h"
#include "encoding.h"

// #define LGELEM 2    // 同时改变
// typedef unsigned int TYPE;
#define LGELEM 3    // 同时改变
typedef unsigned long long TYPE;

typedef unsigned long long uint64;

#define NPE 64
#define SIZE 128 //64
#define NM 16384 // 4096
TYPE DATA[NM]   = {0};  // todo：二维数组为什么不可以使用
TYPE SYMMAT[NM] = {0};
TYPE MEAN[SIZE] = {0};

#include "common.h"
#include "lib.h"
#include "covariance.h"

#endif
