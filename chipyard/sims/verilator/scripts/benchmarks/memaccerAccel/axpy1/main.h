#ifndef _main_h
#define _main_h

#include<stdio.h>
#include "rocc.h"
#include "encoding.h"

#define LGELEM 2    // 同时改变
typedef unsigned int TYPE;

typedef unsigned long long uint64;

#define NPE 64
#define MAXSIZE 4096 //16384 // 4096

TYPE A[MAXSIZE] = {0};
TYPE B[MAXSIZE] = {0};
// TYPE C[MAXSIZE] = {0};

#include "common.h"
#include "lib.h"
#include "axpy.h"

#endif
