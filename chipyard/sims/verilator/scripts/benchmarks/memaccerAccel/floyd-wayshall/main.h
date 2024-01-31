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

#define SIZE 64
#define MM 4096
// #define SIZE 128
// #define MM 16384
TYPE PATH[MM] = {0};

#include "common.h"
#include "lib.h"
#include "floyd-warshall.h"

#endif
