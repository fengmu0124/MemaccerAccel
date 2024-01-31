#ifndef _main_h
#define _main_h

#include<stdio.h>
#include "rocc.h"
#include "encoding.h"

#define LGELEM 2    // 同时改变
typedef unsigned int TYPE;

typedef unsigned long long uint64;

#define NPE 64
#define N 32       // 样本个数
#define M 512      // 样本属性
#define K 2        // 中心个数
#define KM  1024
#define FLT_MAX 3.4e38


TYPE c[KM]      = {0};
TYPE cc[KM]     = {0};
TYPE labels[N]  = {0};

#include "minst.h"

#include "common.h"
#include "lib.h"
#include "kmean.h"

#endif
