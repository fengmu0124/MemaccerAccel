package memaccer

import chisel3._                        // VecInit
import chisel3.util._                   // MuxCase
import freechips.rocketchip.config.Parameters


class AccelReqBundle(val n: Int, val w: Int, val n_op: Int) extends Bundle{
    val src0 = Vec(n, UInt(w.W))
    val src1 = Vec(n, UInt(w.W))
    val op = UInt(n_op.W)
    val scalaA = UInt(64.W)
    val scalaB = UInt(64.W)
}

class AccelRespBundle(val n: Int, val w: Int) extends Bundle{
    val res = Vec(n, UInt(w.W))
    val value = UInt(64.W)
}

class ALUs(n: Int, w: Int, n_op: Int) extends Module{

    val io = IO(new Bundle{
        val req = Flipped(Decoupled(new AccelReqBundle(n, w, n_op)))       
        val resp = Decoupled(new AccelRespBundle(n, w))
    })

    val src0 = RegInit(VecInit(Seq.fill(n)(0.S(w.W))))
    val src1 = RegInit(VecInit(Seq.fill(n)(0.S(w.W))))
    val scalaA = RegInit(0.S(64.W))
    val scalaB = RegInit(0.S(64.W))
    val op = RegInit(0.U(n_op.W))
    val res = RegInit(VecInit(Seq.fill(n)(0.S(w.W))))
    val treeRes = RegInit(0.S(64.W))
    val mask    = RegInit(VecInit(Seq.fill(64){false.B}))

    val sIDLE :: sCAL :: sRESP :: Nil = Enum(3)
    val state = RegInit(sIDLE)
    val flag = RegInit(false.B)     // 乘加求和

    io.req.ready := false.B
    io.resp.valid := false.B     
    io.resp.bits.res := res.asTypeOf(Vec(n, UInt(w.W)))
    io.resp.bits.value := Mux(op===4.U, treeRes.asUInt, mask.asUInt)

    switch(state){
        is(sIDLE){
            io.req.ready := true.B 
            src0    := io.req.bits.src0.asTypeOf(Vec(n, SInt(w.W)))
            src1    := io.req.bits.src1.asTypeOf(Vec(n, SInt(w.W)))
            scalaA  := io.req.bits.scalaA.asSInt
            scalaB  := io.req.bits.scalaB.asSInt
            op      := io.req.bits.op
            state   := Mux(io.req.fire(), sCAL, sIDLE)
            flag    := false.B 
        }
        is(sCAL){
            // TODO n 太大时可以考虑分片计算
            switch(op){
                is(0.U){// "+"
                    for(i <- 0 until n){
                        res(i) := scalaA * src0(i) + scalaB * src1(i)
                    }
                    state := sRESP
                }
                is(1.U){// "-"
                    for(i <- 0 until n){
                        res(i) := scalaA * src0(i) - scalaB * src1(i)
                    }
                    state := sRESP
                }
                is(2.U){// "*"
                    for(i <- 0 until n){
                        res(i) := (scalaA * src0(i)) * (scalaB * src1(i))
                    }
                    state := sRESP
                }
                is(3.U){// "/"
                    for(i <- 0 until n){
                        res(i) := (scalaA*src0(i)) / (scalaB*src1(i))
                    }
                    state := sRESP
                }
                is(4.U){ // 乘加求和                    
                    when(!flag){
                        for(i <- 0 until n){
                            res(i) := (scalaA * src0(i)) * (scalaB * src1(i))
                        }
                        flag := true.B 
                    }.otherwise{
                        when(n.U === 64.U){
                            treeRes :=  res(0) + res(1) + res(2) + res(3) + res(4) + res(5) + res(6) + res(7) +
                                        res(8) + res(9) + res(10) + res(11) + res(12) + res(13) + res(14) + res(15) +
                                        res(16) + res(17) + res(18) + res(19) + res(20) + res(21) + res(22) + res(23) +
                                        res(24) + res(25) + res(26) + res(27) + res(28) + res(29) + res(30) + res(31) +
                                        res(32) + res(33) + res(34) + res(35) + res(36) + res(37) + res(38) + res(39) +
                                        res(40) + res(41) + res(42) + res(43) + res(44) + res(45) + res(46) + res(47) +
                                        res(48) + res(49) + res(50) + res(51) + res(52) + res(53) + res(54) + res(55) +
                                        res(56) + res(57) + res(58) + res(59) + res(60) + res(61) + res(62) + res(63)
                        } 
                        state := sRESP
                        flag := false.B
                    }     
                } 
                is(5.U){
                    for(i <- 0 until 64){
                        mask(i) := src0(i) < src1(i)
                    }
                    state := sRESP
                }                 
            }// end of switch(op)
            
        }
        is(sRESP){
            io.resp.valid := true.B
            state := Mux(io.resp.fire(), sIDLE, sRESP)
        }
    }
}
