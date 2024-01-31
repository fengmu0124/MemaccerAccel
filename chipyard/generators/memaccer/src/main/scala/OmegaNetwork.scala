package memaccer

import chisel3._
import chisel3.util._

class Switch(w: Int) extends Module{
    // 最高位1时，交叉，反之直连
    val io = IO(new Bundle{
        val in0 = Input(UInt(w.W))
        val in1 = Input(UInt(w.W))
        val out0 = Output(UInt((w-1).W))
        val out1 = Output(UInt((w-1).W))
    })
    val nextIn0 = io.in0(w-2, 0) 
    val nextIn1 = io.in1(w-2, 0)
    io.out0 := Mux(io.in0(w-1) === 1.U, nextIn1, nextIn0)
    io.out1 := Mux(io.in0(w-1) === 1.U, nextIn0, nextIn1)
}

// Omega64 network
class Omega(w: Int) extends Module{
    val n       = 64
    val nstage  = log2Ceil(n)
    
    val io = IO(new Bundle{
        val in = Input(Vec(n, UInt(w.W)))
        val out = Output(Vec(n, UInt(w.W)))
        val shift = Input(UInt(nstage.W))
    })
    
    val stage5 = Seq.fill(n/2){Module(new Switch(6+w))}
    val stage4 = Seq.fill(n/2){Module(new Switch(5+w))}
    val stage3 = Seq.fill(n/2){Module(new Switch(4+w))}
    val stage2 = Seq.fill(n/2){Module(new Switch(3+w))}
    val stage1 = Seq.fill(n/2){Module(new Switch(2+w))}
    val stage0 = Seq.fill(n/2){Module(new Switch(1+w))}
    
    // stage5
    for(i <- 0 until n){      
        val j = shuffle(i, n)        
        val d = (i.U + io.shift)(nstage-1, 0)  // 目标地址
        if(j%2 == 0){ // upper
        stage5(j/2).io.in0 := Cat(d, io.in(i))
        }else{        // lower
        stage5(j/2).io.in1 := Cat(d, io.in(i))
        }
    }
    
    // stage4
    for(i <- 0 until n){      
        val j = shuffle(i, n)
        if(j%2 == 0){
        stage4(j/2).io.in0 := (if(i%2 == 0) stage5(i/2).io.out0 else stage5(i/2).io.out1)
        }else{
        stage4(j/2).io.in1 := (if(i%2 == 0) stage5(i/2).io.out0 else stage5(i/2).io.out1)
        }
    }

    // stage3
    for(i <- 0 until n){      
        val j = shuffle(i, n)
        if(j%2 == 0){
        stage3(j/2).io.in0 := (if(i%2 == 0) stage4(i/2).io.out0 else stage4(i/2).io.out1)
        }else{
        stage3(j/2).io.in1 := (if(i%2 == 0) stage4(i/2).io.out0 else stage4(i/2).io.out1)
        }
    }
    
    // stage2
    for(i <- 0 until n){      
        val j = shuffle(i, n)
        if(j%2 == 0){
        stage2(j/2).io.in0 := (if(i%2 == 0) stage3(i/2).io.out0 else stage3(i/2).io.out1)
        }else{
        stage2(j/2).io.in1 := (if(i%2 == 0) stage3(i/2).io.out0 else stage3(i/2).io.out1)
        }
    }

    // stage1
    for(i <- 0 until n){      
        val j = shuffle(i, n)
        if(j%2 == 0){
        stage1(j/2).io.in0 := (if(i%2 == 0) stage2(i/2).io.out0 else stage2(i/2).io.out1)
        }else{
        stage1(j/2).io.in1 := (if(i%2 == 0) stage2(i/2).io.out0 else stage2(i/2).io.out1)
        }
    }
        
    // stage0
    for(i <- 0 until n){      
        val j = shuffle(i, n)
        if(j%2 == 0){
        stage0(j/2).io.in0 := (if(i%2 == 0) stage1(i/2).io.out0 else stage1(i/2).io.out1)
        }else{
        stage0(j/2).io.in1 := (if(i%2 == 0) stage1(i/2).io.out0 else stage1(i/2).io.out1)
        }
        io.out(i) := (if(i%2 == 0) stage0(i/2).io.out0 else stage0(i/2).io.out1)
    } 

    def shuffle(i : Int, n: Int) = ((i << 1) & (n-1)) | (i >> (log2Ceil(n)-1)) 

}