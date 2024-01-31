package memaccer

import chisel3._                        // VecInit
import chisel3.util._                   // MuxCase
import freechips.rocketchip.tile._      // LazyRoCC
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._ // LazyModule
import freechips.rocketchip.rocket._

class ReqBundle(nPE: Int) extends Bundle{
    val opType      = UInt(4.W)         // op_LOAD、op_STORE、op_COMPUTE、op_FLUSH
    val addr        = UInt(64.W)
    val vmask       = Vec(nPE, Bool())
    val lgElem      = UInt(2.W)
    val elems       = UInt(16.W)
    val stride      = UInt(16.W)
    val flush       = UInt(1.W)         // 0 dirty 写回， 1 valid 无效 dirty写回
    val calOp       = UInt(3.W)         // +-*/ 、乘加求和、比较
    val scalaA      = UInt(64.W)
    val scalaB      = UInt(64.W)
    val loadAddr    = UInt(2.W)         // load 数据保存的向量寄存器位置 0 src0, 1 src1
    val way         = UInt(2.W)         // 0 DMA+缓存, 1 DMA, 2 cache
    //after determined the access patten, we can define the require input as[read/write(op), elemType, elemNum, baseAddr. stride, mask]

    //val valid       = Bool

    // @lyq modify
    //override def cloneType: this.type = new ReqBundle(nPE).asInstanceOf[this.type]
    // new ReqBundle(nPE).asInstanceOf[this.type]
}

class RoCCMemaccerAccel(opcode: OpcodeSet)(
    implicit p: Parameters) extends LazyRoCC(opcode){
    
    import ScratchpadConfig._

    val spm = LazyModule(new ScratchpadMgt)
    //override val tlNode = spm.dmaNode
    tlNode := spm.dmaNode

    override lazy val module = new LazyRoCCModuleImp(this){

        val vregs_src0  = RegInit(VecInit(Seq.fill(nbank)(1.U(wbankElem.W)))) 
        val vregs_src1  = RegInit(VecInit(Seq.fill(nbank)(1.U(wbankElem.W)))) 
        val vregs_res   = RegInit(VecInit(Seq.fill(nbank)(1.U(wbankElem.W)))) 
        val calResValue     = RegInit(0.U(64.W))
        //@lyq cihselTypeof wire io
        //val req         = Reg(chiselTypeOf(IO(new ReqBundle(nbank))))
        val req         = Reg(new ReqBundle(nbank))

        // 译码RoCC指令
        val rd = RegInit(0.U(5.W)) 
        val busy = RegInit(false.B)
        io.cmd.ready        := !busy
        io.busy             := busy
        io.resp.valid       := false.B 
        io.resp.bits.rd     := rd
        io.resp.bits.data   := calResValue



        val sIDLE :: sHANDLE :: sDONE :: Nil = Enum(3) 
        val state = RegInit(sIDLE)
        val op_LOAD :: op_STORE :: op_COMPUTE :: op_FLUSH :: Nil = Enum(4)
        val canPrint = RegInit(false.B)        

        val spmReadIO = spm.module.io.read
        spmReadIO.req.valid         := false.B 
        spmReadIO.req.bits.addr     := req.addr
        spmReadIO.req.bits.lgElem   := req.lgElem
        spmReadIO.req.bits.elems    := req.elems
        spmReadIO.req.bits.stride   := req.stride
        spmReadIO.req.bits.bypass   := req.way === 1.U 
        spmReadIO.resp.ready        := false.B 

        val spmWriteIO = spm.module.io.write 
        spmWriteIO.valid        := false.B
        spmWriteIO.bits.addr    := req.addr
        spmWriteIO.bits.lgElem  := req.lgElem
        spmWriteIO.bits.elems   := req.elems
        spmWriteIO.bits.stride  := req.stride
        spmWriteIO.bits.bypass  := req.way === 1.U 
        spmWriteIO.bits.vmask   := req.vmask
        spmWriteIO.bits.vregs   := vregs_res

        val spmFlushIO = spm.module.io.flush
        spmFlushIO.valid    := false.B 
        spmFlushIO.bits     := req.flush

        val alus = Module(new ALUs(nbank, wbankElem, 3))
        alus.io.req.valid       := false.B 
        alus.io.req.bits.src0   := vregs_src0
        alus.io.req.bits.src1   := vregs_src1
        alus.io.req.bits.scalaA := req.scalaA
        alus.io.req.bits.scalaB := req.scalaB
        alus.io.req.bits.op     := req.calOp

        alus.io.resp.ready      := false.B

        val l1_IDLE :: l1_OP :: Nil = Enum(2)
        val l1state = RegInit(l1_IDLE)
        //@ lyq move form dma load
        val lIDLE :: lRESP :: Nil = Enum(2)
        val lstate = RegInit(lIDLE)

        val elemMasks = VecInit(0xff.S(64.W), 0xffff.S(64.W), 0xffffffff.S(64.W), ~(0.S(64.W)))
        val idx = RegInit(0.U(log2Ceil(nbank+1).W))
        val i = idx(wnbank-1, 0)
        //cache link
        val l1req = io.mem.req
        l1req.valid         := false.B
        l1req.bits.addr     := req.addr
        l1req.bits.cmd      := Mux(req.opType === 0.U, M_XRD, M_XWR)

        l1req.bits.idx.foreach(_ := req.addr)
        l1req.bits.size     := req.lgElem
        l1req.bits.data     := vregs_res(i)
        l1req.bits.signed   := false.B
        l1req.bits.phys     := false.B
        l1req.bits.dprv     := io.cmd.bits.status.dprv
        //@lyq dv
        l1req.bits.dv       := io.cmd.bits.status.dv

        //l1req.bits.idx
        //io.mem.resp.valid
        val l1resp = io.mem.resp    // Valid 接口
        //val s2_xcpt = io.mem.s2_xcpt
        //l1resp.valid        := io.mem.resp.valid

        when(l1resp.valid){
            printf("@l1resp: addr=[%x]\t tag=[%x]\t data=[%x]\t cmd=[%x]\n",l1resp.bits.addr,l1resp.bits.tag,l1resp.bits.data,l1resp.bits.cmd)
        }.elsewhen(io.mem.s2_xcpt.asUInt.orR){
            printf("@io.mem: addr=[%x]\t tag=[%x]\t data=[%x]\t cmd=[%x]\t s2_xcpt=[%x]\n",l1resp.bits.addr,l1resp.bits.tag,l1resp.bits.data,l1resp.bits.cmd,io.mem.s2_xcpt.asUInt)
        }
        //printf("@io.mem.resp.data = [%x]\t io.mem.resp.addr = [%x]\n",io.mem.resp.bits.data,io.mem.resp.bits.addr)
        //printf("@l1req: l1req.valid = %d\tl1req.bits = %lld",l1req.valid, l1req.bits.asUInt)

        switch(state){
            is(sIDLE){
                //printf("enter sIDLE\n")
                //printf("@l1req: l1req.valid = %d\tl1req.cmd = %d\tl1req.addr = %d\n",l1req.valid, l1req.bits.cmd, l1req.bits.addr)
                //printf("@io.mem.req=[%x]\t l1req.bits.cmd = [%x]\n",io.mem.req.asUInt,l1req.bits.cmd)
                //printf("@io.mem.req.ready=[%d]\t io.mem.req.valid = [%d]\n",io.mem.req.ready,io.mem.req.valid)
                idx := 0.U                
                when(io.cmd.fire){
                    val rs1 = io.cmd.bits.rs1
                    val rs2 = io.cmd.bits.rs2
                    // @lyq When state is sIDLE turn canPrint to True
                    canPrint := true.B
                    val funct = io.cmd.bits.inst.funct
                    //@lyq
                    printf("funct = [0x%x] io.cmd.bits.inst.funct =[0x%x]\n",funct,io.cmd.bits.inst.funct)
                    when(funct===0.U){
                        busy            := true.B
                        state           := sHANDLE
                        req.opType      := op_LOAD
                        req.addr        := rs1
                        req.lgElem      := rs2(1, 0)
                        req.elems       := rs2(17, 2)
                        req.stride      := rs2(33, 18)                      
                        req.loadAddr    := rs2(35,34)
                        req.way         := rs2(37,36)
                    }.elsewhen(funct===1.U){
                        req.opType      := op_STORE
                        req.addr        := rs1                          
                        req.lgElem      := rs2(1, 0)
                        req.elems       := rs2(17, 2)
                        req.stride      := rs2(33, 18)       
                    }.elsewhen(funct===2.U){                            
                        busy        := true.B  
                        state       := sHANDLE                          
                        req.vmask   := rs1.asTypeOf(Vec(nbank, Bool()))
                    }.elsewhen(funct===3.U){
                        busy        := true.B 
                        state       := sHANDLE
                        req.opType  := op_FLUSH
                        req.flush   := rs1                            
                    }.otherwise{
                        busy        := true.B 
                        state       := sHANDLE
                        req.opType  := op_COMPUTE
                        req.calOp   := funct - 4.U 
                        req.scalaA  := rs1
                        req.scalaB  := rs2
                        rd := io.cmd.bits.inst.rd
                    }
                }// end of when(io.cmd.fire)
                //printf("sIDLE end\n")
            }// end of sIDLE
            is(sHANDLE){
                printf("enter sHandle\n")
                printf("req.onType = %d\n",req.opType)
                switch(req.opType){
                    is(op_LOAD){
                        when(req.way === 2.U){// cache load
                            //L1 Cache load
                            printf("enter L1 Cache load\n")
                            switch(l1state){
                                is(l1_IDLE){
                                    printf("l1_IDLE start\n")
                                    l1req.valid := true.B 
                                    when(l1req.fire){
                                        printf("before shift: req.addr=[%x]\n",req.addr)
                                        printf("req.stride=[%x]\t req.lgElem=[%x]\n",req.stride,req.lgElem)
                                        //@lyq operator problem
                                        //req.addr := req.addr + (req.stride << req.lgElem)
                                        val temp = req.stride.<<(req.lgElem)
                                        val a1 = req.addr + temp:UInt
                                        printf("temp = [%x]\ta1 = [%x]\n",temp,a1)
                                        printf("@io.mem.req.ready=[%d]\t io.mem.req.valid = [%d]\n",io.mem.req.ready,io.mem.req.valid)
                                        //@lyq only ready and valid can refresh the address
                                        req.addr := a1

                                        //req.addr(UInt(16.W)) := a1(UInt(16.W))
                                        //req.addr := req.addr + req.stride.<<(req.lgElem)
                                        printf("after shift: req.addr=[%x]\n",req.addr)
                                        printf("@bits: addr.bits=[%x] width =[%d],\t a1.bits=[%x] width =[%d]\n",req.addr:Bits,req.addr.getWidth.asUInt,a1:Bits,a1.getWidth.asUInt)
                                        l1state := l1_OP
                                    }
                                }
                                is(l1_OP){
                                    printf("l1_OP start\n")
                                    when(l1resp.valid){
                                        idx := idx + 1.U 
                                        l1state := l1_IDLE
                                        val dat = elemMasks(req.lgElem).asUInt & l1resp.bits.data                                        
                                        when(req.loadAddr === 0.U){
                                            vregs_src0(i) := dat
                                            vregs_src1(i) := 1.U    // 将src1置为1实现向量与标量的运算
                                        }.elsewhen(req.loadAddr === 1.U){
                                            vregs_src1(i) := dat
                                        }.otherwise{
                                            vregs_res(i) := dat
                                        }
                                        when(idx === req.elems - 1.U){
                                            state := sDONE
                                        }
                                    }
                                }
                            }
                        }.otherwise{    // dma load
                            printf("dma load\n")
                            //@lyq move out switch session
//                            val lIDLE :: lRESP :: Nil = Enum(2)
//                            val lstate = RegInit(lIDLE)
                            switch(lstate){
                                is(lIDLE){
                                    spmReadIO.req.valid := true.B
                                    lstate := Mux(spmReadIO.req.fire, lRESP, lIDLE)
                                    printf("@lIDLE: spReadIO.req.fire = [%d]\tspmReadIO.req.valid = [%d]\n",spmReadIO.req.fire,spmReadIO.req.valid)
                                }
                                is(lRESP){
                                    spmReadIO.resp.ready := true.B
                                    //@lyq test spmReadIO
                                    printf("@lRESP: spmReadIO.req.valid=[%d]\tspmReadIO.req.ready=[%d]\n",spmReadIO.req.valid,spmReadIO.req.ready)
                                    when(spmReadIO.resp.fire){
                                        val vregs = (spmReadIO.resp.bits.vregs)
                                        printf("@dma req: req.loadAddr = [%d]\n",req.loadAddr)

                                        when(req.loadAddr === 0.U){
                                            vregs_src0 := vregs
                                            vregs_src1 := VecInit(Seq.fill(nbank)(1.U(wbankElem.W)))
                                        }.elsewhen(req.loadAddr === 1.U){
                                            vregs_src1 := vregs
                                        }.otherwise{
                                            vregs_res := vregs
                                        }
                                        lstate := lIDLE
                                        state := sDONE
                                    }
                                }
                            } // end of lstate
                            printf("dma load end\n")
                        }
                        printf("op_LOAD end l1state = %d, lstate = %d\n",l1state,lstate)
                    }// end of op_LOAD

                    is(op_STORE){
                        printf("op_STORE start\n")
                        when(req.way === 2.U){
                            switch(l1state){
                                is(l1_IDLE){
                                    l1req.valid := req.vmask(idx)
                                    when(l1req.fire){
                                        l1state := l1_OP
                                        idx := idx + 1.U 
                                        req.addr := req.addr + (req.stride << req.lgElem)
                                    }
                                }
                                is(l1_OP){
                                    when(l1resp.valid){ 
                                        l1state := l1_IDLE                                       
                                        when(idx === req.elems){
                                            state := sDONE
                                        } 
                                    }
                                }
                            }
                        }.otherwise{
                            spmWriteIO.valid := true.B 
                            state := Mux(spmWriteIO.fire, sDONE, state)
                        }
                        printf("op_STORE end\n")
                    }// end of op_STORE

                    is(op_COMPUTE){
                        printf("op_COMPUTE start\n")
                        alus.io.req.valid   := true.B 
                        alus.io.resp.ready  := true.B 
                        when(alus.io.resp.fire){
                            when(req.calOp === 5.U){// mask计算
                                calResValue := alus.io.resp.bits.value
                            }.elsewhen(req.calOp === 4.U){
                                calResValue := alus.io.resp.bits.value
                            }.otherwise{            // 并行加减乘除
                                printf("do alu\n")
                                vregs_res := alus.io.resp.bits.res
                            }                            
                            state := sDONE
                        }
                        printf("op_COMPUTE end\n")
                    }// end of op_COMPUTE

                    is(op_FLUSH){
                        printf("op_FLUSH start\n")
                        spmFlushIO.valid := true.B 
                        state := Mux(spmFlushIO.fire, sDONE, state)
                        printf("op_FLUSH end\n")
                    }
                }
                // @lyq execute canPrint state while turn off canPrint, which means the sHANDLE is over.
                when(canPrint){
                    canPrint := false.B
                    printf("@RMA: req info...\n")    
                    printf("opType=%d, way=%d flush=%d, calOp=%d, scalaA=%d, scalaB=%d\n", 
                        req.opType, req.way, req.flush, req.calOp, req.scalaA, req.scalaB)
                    printf("addr=%x\t, lgElem=%d\t, elems=%d, stride=%d, vmask=%x, loadAddr=%d\n", 
                        req.addr, req.lgElem, req.elems, req.stride, req.vmask.asUInt, req.loadAddr)
                }
            }// end of sHANDLE
            
            is(sDONE){
                printf("enter sDONE\n")
                io.resp.valid := true.B                
                when(io.resp.fire){
                    busy := false.B 
                    state := sIDLE
                    printf("@MTP: result(%d)...\n", calResValue)
                    when(req.opType =/= op_FLUSH)
                    {
                        printf("vregs_src0 result: \n")
                        for(i <- 0 until nbank){
                            printf("%d\t", vregs_src0(i))
                            if((i+1)%8 == 0){
                                printf("\n")
                            }
                        }
                        printf("vregs_src1 result: \n")
                        for(i <- 0 until nbank){
                            printf("%d\t", vregs_src1(i))
                            if((i+1)%8 == 0){
                                printf("\n")
                            }
                        }
                        printf("vregs_res result: \n")
                        for(i <- 0 until nbank){
                            printf("%d\t", vregs_res(i))
                            if((i+1)%8 == 0){
                                printf("\n")
                            }
                        }
                    }
                }// end of io.resp.fire
            }// end of sDONE
        }// end of state
        //printf("state = %d\n",state)
        //printf("---------------------------------state end---------------------------------\n")
    }// end of LazyRoCCModuleImp
}


// 带有向量加速器的测试平台
class WithMemaccerAccel extends Config((site,here,up) => {
    case BuildRoCC => Seq(       
        (p:Parameters) => {
            val rocc = LazyModule(new RoCCMemaccerAccel(OpcodeSet.all)(p))
            rocc
        }
    )
})


//object PrintTester extends App {
//    println("Start Testing")
//}