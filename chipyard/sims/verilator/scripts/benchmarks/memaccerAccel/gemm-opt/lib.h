#ifndef _lib_h
#define _lib_h

int genConfig_Count = 0;

uint64 genConfig(
	uint64 elems, 		// 元素总个数
	uint64 stride, 		// 1 为连续访存
	uint64 loadSrc,		// load 到src0/src1	
	uint64 way			// 0 DMA+缓存, 1 DMA, 2 cache
){
	genConfig_Count++;
	return 	(way & 0x03) << 36		|	// 2bit
			(loadSrc & 0x03) << 34	|	// 2bit
			(stride & 0xffff) << 18	|	// 16 bits
			(elems & 0xffff) << 2	|	// 16 bits
			(LGELEM & 0x03);			// 2 bits
}

inline void  load(
    uint64 baseAddr, 
    uint64 stride, 
    uint64 elems, 
    uint64 loadSrc, 
    uint64 way){	

	uint64 config = genConfig(elems, stride, loadSrc, way);	

	asm volatile("fence");
	ROCC_INSTRUCTION_SS(0, baseAddr, config, 0);
	asm volatile("fence":::"memory");  

}

inline void store(
    uint64 baseAddr, 
    uint64 stride, 
    uint64 elems, 
    uint64 vmask, 
    uint64 way){

	uint64 config = genConfig(elems, stride, 2, way);

	asm volatile("fence");
	ROCC_INSTRUCTION_SS(0, baseAddr, config, 1);
	asm volatile("fence":::"memory");

	asm volatile("fence");
	ROCC_INSTRUCTION_S(0, vmask, 2);
	asm volatile("fence":::"memory"); 

}

inline uint64 compute(
    uint64 op, //0-4： +-*/ 和 乘加求和
    uint64 scalaA, 
    uint64 scalaB){
	uint64 rd;
    if(op == 0){	
	    asm volatile("fence");
	    ROCC_INSTRUCTION_DSS(0, rd, scalaA, scalaB, 4);// +
	    asm volatile("fence":::"memory"); 
    }else if(op == 1){	
        asm volatile("fence");
        ROCC_INSTRUCTION_DSS(0, rd, scalaA, scalaB, 5); // -
        asm volatile("fence":::"memory");
    }else if(op ==2){	
        asm volatile("fence");
        ROCC_INSTRUCTION_DSS(0, rd, scalaA, scalaB, 6);// *
        asm volatile("fence":::"memory");
    }else if(op ==3){	
        asm volatile("fence");
        ROCC_INSTRUCTION_DSS(0, rd, scalaA, scalaB, 7);// /
        asm volatile("fence":::"memory"); 
    }else if(op ==4){	
        asm volatile("fence");
        ROCC_INSTRUCTION_DSS(0, rd, scalaA, scalaB, 8);// 乘加求和
        asm volatile("fence":::"memory"); 
    } 
       
	return rd;	
}

inline void flush(uint64 op){
	// op 0 dirty 写回， 1 dirty 写回 valid复位

	asm volatile("fence");
	ROCC_INSTRUCTION_S(0, op, 3);
	asm volatile("fence":::"memory");
	
}

void dload(
    uint64 baseAddr, 
    uint64 stride, 
    uint64 elems, 
    uint64 loadSrc, 
    uint64 way){
	
    uint64 start = 0, cycles = 0;
	uint64 config = genConfig(elems, stride, loadSrc, way);	
	printf("start load(addr%llx, config%llx)...\n", baseAddr, config);	
	
	start = rdcycle();
	asm volatile("fence");
	ROCC_INSTRUCTION_SS(0, baseAddr, config, 0);
	asm volatile("fence":::"memory");  
	cycles = rdcycle() - start;

	printf("finish load! cost %llu cycles\n", cycles);
}

void dstore(
    uint64 baseAddr, 
    uint64 stride, 
    uint64 elems, 
    uint64 vmask, 
    uint64 way){

	uint64 start = 0, cycles = 0;		
	uint64 config = genConfig(elems, stride, 2, way);
	printf("start store(addr%llx, config%llx)...\n", baseAddr, config);	
	
	start = rdcycle();
	asm volatile("fence");
	ROCC_INSTRUCTION_SS(0, baseAddr, config, 1);
	asm volatile("fence":::"memory");
	asm volatile("fence");
	ROCC_INSTRUCTION_S(0, vmask, 2);
	asm volatile("fence":::"memory"); 
	cycles = rdcycle() - start;		
	
	printf("finish store! cost %llu cycles\n", cycles);
}

uint64 dcompute(
    uint64 op, //0-4： +-*/ 和 乘加求和
    uint64 scalaA, 
    uint64 scalaB){

	uint64 start = 0, cycles = 0, rd = 0;
	printf("start compute %s...\n",op==0 ? "add" :(op==1 ? "subtract" :(op==2 ? "product" :(op==3 ? "divide" : "product-add-sum"))));

    if(op == 0){
        start = rdcycle();	
	    asm volatile("fence");
	    ROCC_INSTRUCTION_DSS(0, rd, scalaA, scalaB, 4);// +
	    asm volatile("fence":::"memory");  
	    cycles = rdcycle() - start;
    }else if(op == 1){
        start = rdcycle();	
        asm volatile("fence");
        ROCC_INSTRUCTION_DSS(0, rd, scalaA, scalaB, 5); // -
        asm volatile("fence":::"memory");  
        cycles = rdcycle() - start;
    }else if(op ==2){
        start = rdcycle();	
        asm volatile("fence");
        ROCC_INSTRUCTION_DSS(0, rd, scalaA, scalaB, 6);// *
        asm volatile("fence":::"memory");  
        cycles = rdcycle() - start;
    }else if(op ==3){
        start = rdcycle();	
        asm volatile("fence");
        ROCC_INSTRUCTION_DSS(0, rd, scalaA, scalaB, 7);// /
        asm volatile("fence":::"memory");  
        cycles = rdcycle() - start;
    }else if(op ==4){
        start = rdcycle();	
        asm volatile("fence");
        ROCC_INSTRUCTION_DSS(0, rd, scalaA, scalaB, 8);// 乘加求和
        asm volatile("fence":::"memory");  
        cycles = rdcycle() - start;
    }   
	printf("finish compute! cost %llu cycles\n", cycles);
	return rd;
}

void dflush(uint64 op){// op 0 dirty 写回， 1 dirty 写回 valid复位
	uint64 start = 0, cycles = 0;
	printf("start flush %s...\n", op==0 ? "dirty" : "all");
	
    start = rdcycle();	
	asm volatile("fence");
	ROCC_INSTRUCTION_S(0, op, 3);
	asm volatile("fence":::"memory");
	cycles = rdcycle() - start;	

	printf("finish flush! cost %llu cycles\n", cycles);
}

#endif