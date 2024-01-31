package memaccer

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLIdentityNode, TLXbar}


// 加速器端口
class AcceleratorReadReq extends Bundle{
    val addr    = UInt(64.W)
    val lgElem  = UInt(2.W)     // 0: char, 1:UInt16, 2:UInt32/float, 3:UInt64/double,lgElem是数据类型
    val elems   = UInt(16.W)    // 读取的元素个数          
    val stride  = UInt(16.W)    // 读取的步长        
    val bypass  = Bool()        // 直接访问DRAM,不经过scratchpad                  
}

class AcceleratorReadResp(val n:Int, val w:Int) extends Bundle{
    val vregs = Vec(n, UInt(w.W))                                                                    // 读响应数据
    override def cloneType: this.type = new AcceleratorReadResp(n, w).asInstanceOf[this.type]        // 
}

class AcceleratorReadIO(val n:Int, val w:Int) extends Bundle{
    val req     = Decoupled(new AcceleratorReadReq)                                                 // 读请求
    val resp    = Flipped(Decoupled(new AcceleratorReadResp(n, w)))                                 // 读响应
}

class AcceleratorWriteIO(val n:Int, val w:Int) extends Bundle{
    val addr    = UInt(64.W)
    val lgElem  = UInt(2.W)              // 0: char, 1:UInt16, 2:UInt32/float, 3:UInt64/double
    val elems   = UInt(16.W)              
    val stride  = UInt(16.W)         
    val vmask   = Vec(n, Bool())         // 1 有效，0 不变
    val vregs   = Vec(n, UInt(w.W))      // vregs是写入的数据
    val bypass  = Bool()                 // 直接访问DRAM，是从scratchpad写入DRAM
    override def cloneType: this.type = new AcceleratorWriteIO(n, w).asInstanceOf[this.type] 
}

class PatternBundle extends Bundle{
    val addr    = UInt(64.W)    // 物理地址通常为32位
    val lgElem  = UInt(2.W)
    val stride  = UInt(16.W)    // 步长宽度通常设置成16位
}// 模式
    

// scratchpad 实现
class ScratchpadMgt(implicit p: Parameters) extends LazyModule{

    import ScratchpadConfig._

    val dmaCtrl = LazyModule(new DMAController)
    val dmaNode = dmaCtrl.node                      // 需要挂到RoCC的tlNode

    lazy val module = new LazyModuleImp(this){
        val io = IO(new Bundle{
            val read    = Flipped(new AcceleratorReadIO(nbank, wbankElem))
            val write   = Flipped(Decoupled(new AcceleratorWriteIO(nbank, wbankElem)))
            val busy    = Output(Bool())            
            val flush   = Flipped(Decoupled(Bool())) // 0: dirty 写回， 1： dirty写回valid无效
        }) 

        // 主状态机
        val sIDLE :: sBYPASS :: sCACHE :: sRESP :: sFLUSH :: Nil = Enum(5)
        val state = RegInit(sIDLE) 
        // SPM访问状态机
        val cIDLE :: cQUERY :: cDMA :: cRESP :: Nil = Enum(4)
        val cstate = RegInit(cIDLE)   
        // DMA访问状态机
        val rDMA_IDLE :: rDMA_WAIT ::Nil = Enum(2)
        val DMA_state = RegInit(rDMA_IDLE)
        // SPM miss 写回状态机
        val dIDLE :: dWBACK :: dLOAD :: Nil = Enum(3)
        val dstate = RegInit(dIDLE)
        // flush 状态机
        val fIDLE :: fWBLOCK :: Nil = Enum(2)
        val fstate = RegInit(fIDLE)

        io.busy := !(state === sIDLE)        

        val ren         = RegInit(false.B)
        val addr        = RegInit(0.U(64.W))
        val lgElem      = RegInit(0.U(2.W))
        val elems       = RegInit(0.U(16.W))
        val stride      = RegInit(0.U(16.W))
        val bypass      = RegInit(false.B)
        val vregs       = RegInit(VecInit(Seq.fill(nbank)(0.U(wbankElem.W))))
        val vregsValid  = RegInit(false.B)
        val cRegs = RegInit(VecInit(Seq.fill(nbank)(0.U(wbankElem.W))))
        val cRegsValid = RegInit(false.B)
        val vmask       = RegInit(VecInit(Seq.fill(nbank)(true.B)))  
        
  

        val elemMask = VecInit(0xff.U(nbank.W), 0xffff.U(nbank.W), Cat(0xffff.U, 0xffff.U), ~(0.U(nbank.W)))   
        val idx = RegInit(0.U(16.W))   // idx的值可以达到nbank
        val idxNbank = idx(log2Ceil(nbank)-1, 0)
        val totalBytes = elems << lgElem
        val leftBytes = totalBytes - (idx << 3.U)       // TODO: DMA dataR数据宽度值参数化

        val banks = Seq.fill(nbank){Module(new ScratchpadBank(bankElems, wbankElem))}
        val banks_io = VecInit(banks.map(_.io))
        banks_io.foreach{bio =>
            bio.read.req.valid := false.B 
            bio.read.req.bits.addr := DontCare
            bio.read.resp.ready := false.B 
            bio.write.en := false.B 
            bio.write.data := DontCare
            bio.write.addr := DontCare
            bio.write.mask := (0.U).asTypeOf(Vec(bankElemBytes, Bool()))
        }
        val table = Reg(Vec(nblock, new PatternBundle))
        val tabValid = RegInit(VecInit(Seq.fill(nblock)(false.B)))
        val tabDirty = RegInit(VecInit(Seq.fill(nblock)(false.B)))
        val randNum = RegInit(0.U(16.W))
        randNum := randNum + 1.U        // 利用时钟参数一个近似伪随机数

        io.read.req.ready := false.B
        io.read.resp.valid := false.B
        io.write.ready := false.B
        val dmareqio = dmaCtrl.module.io.req
        dmareqio.valid := false.B 
        val dmaDataR = dmaCtrl.module.io.dataR
        dmaDataR.ready := false.B
        val dmaDataW = dmaCtrl.module.io.dataW
        dmaDataW.valid := false.B

        io.flush.ready := false.B 

        val network = Module(new Omega(wbankElem))//wbankElem是bank的宽度
        network.io.in       := DontCare
        network.io.shift    := DontCare
        
        // 固定DMA 设计空间探索的参数
        dmareqio.bits.rid := 0.U
        dmareqio.bits.idxMod := 0.U        // TODO: 写回时vmask模式下使用idxMod实现
        dmareqio.bits.maxReqBytes := 3.U
        dmareqio.bits.strideMod := 0.U   
        dmareqio.bits.ren := ren
        dmareqio.bits.baseAddr := addr
        dmareqio.bits.lgElem := lgElem
        dmareqio.bits.elems := elems
        dmareqio.bits.stride := stride
        dmareqio.bits.vmask := vmask

        val canResp = Wire(Bool())
        canResp := false.B          // 请求处理完成，待返回加速器端
        val done = Wire(Bool())     // 请求响应完成
        done := false.B
        val flushDone = Wire(Bool())     // 请求响应完成
        flushDone := false.B
        state := MuxCase(state, Seq(
            (state === sIDLE && io.flush.valid)                              -> sFLUSH,  // 写回SPM中脏数据
            (((io.read.req.fire() && io.read.req.bits.bypass) || 
            (io.write.valid && io.write.bits.bypass)) && 
            state === sIDLE)                                                 -> sBYPASS,// 请求有DMA处理
            (((io.read.req.fire() && !io.read.req.bits.bypass) || 
            (io.write.valid && !io.write.bits.bypass)) &&
            state === sIDLE)                                                -> sCACHE,  // 请求multi-banks处理          
            ((state === sBYPASS || state === sCACHE) && canResp)            -> sRESP,   // 等待数据被accelerator读取
            ((state === sRESP && done) || (state === sFLUSH && flushDone))   -> sIDLE))

        switch(state){
            is(sIDLE){
                //start: 初始化参数
                idx     := 0.U
                //end: 初始化参数
                
                //start: 读请求
                io.read.req.ready := io.read.req.valid
                when(io.read.req.valid){
                    val r = io.read.req.bits
                    ren := true.B
                    addr := r.addr
                    lgElem := r.lgElem
                    elems := r.elems    // DMA 访问一个block                      
                    stride := r.stride
                    bypass := r.bypass 
                    // printf("@SPM: addr=%x, lgElem=%x, elems=%d, stride=%d, bypass=%d\n",
                    //     r.addr, r.lgElem, r.elems, r.stride, r.bypass)
                }
                //end: 读请求
                
                //start: 写请求
                when(io.write.valid){
                    val w = io.write.bits
                    ren         := false.B
                    addr        := w.addr
                    lgElem      := w.lgElem
                    elems       := w.elems
                    stride      := w.stride
                    bypass      := w.bypass
                    vmask       := w.vmask    
                    vregs       := VecInit(Seq.fill(nbank)(0.U(wbankElem.W)))       // 初始化vregs,初始化为0；vregs是一个向量，向量的每个元素是一个寄存器   
                    // 密集存储处理
                    when(w.lgElem === 0.U){
                    for(i <- 0 until nbank/8){      
                        vregs(i) := w.vregs(i*8) | (w.vregs(i*8 + 1) << 8.U) |
                                (w.vregs(i*8 + 2) << 16.U) | (w.vregs(i*8 + 3) << 24.U) |
                                (w.vregs(i*8 + 4) << 32.U) | (w.vregs(i*8 + 5) << 40.U) |
                                (w.vregs(i*8 + 6) << 48.U) | (w.vregs(i*8 + 7) << 56.U)
                    }
                    }.elsewhen(w.lgElem === 1.U){
                    for(i <- 0 until nbank/4){
                        vregs(i) := w.vregs(i*4) | (w.vregs(i*4 + 1) << 16.U) |
                                (w.vregs(i*4 + 2) << 32.U) | (w.vregs(i*4 + 3) << 48.U)
                    }
                    }.elsewhen(w.lgElem === 2.U){
                    for(i <- 0 until nbank/2){
                        vregs(i) := w.vregs(i*2) | (w.vregs(i*2 + 1) << 32.U)                        
                    }
                    }.otherwise{            
                            vregs := w.vregs               
                    }            
                       
                    // printf("@SPM: addr=%x, lgElem=%x, elems=%d, stride=%d, bypass=%d, vmask=%x\n",
                    //     w.addr, w.lgElem, w.elems, w.stride, w.bypass, w.vmask.asUInt)
                    // printf("@SPM: write result\n")
                    // for(i <- 0 until nbank){                       // 输出前 32个元素，检测读取结果
                    //     printf("%x\t", w.vregs(i).asUInt)
                    //     if((i+1) % 8 == 0){
                    //         printf("\n")
                    //     }
                    // }    
                }
                //end: 写请求                
            }
            is(sBYPASS){
                dmareqio.valid := DMA_state === rDMA_IDLE 
                when(dmareqio.fire()){
                    DMA_state := rDMA_WAIT
                }
                // start: DMA 读取数据
                
                dmaDataR.ready := ren === 1.U && DMA_state === rDMA_WAIT
                when(dmaDataR.fire()){
                    vregs(idx) := dmaDataR.bits
                    idx := idx + 1.U
                    when(leftBytes <= 8.U){
                        DMA_state := rDMA_IDLE
                        canResp := true.B           // state 的下个状态将会切换到 sRESP                       
                    }
                }
                // end DMA 读取数据

                // start: DMA 写数据
                dmaDataW.bits := vregs(idx)        
                when(state === sBYPASS && ren === 0.U){                 
                    dmaDataW.valid := DMA_state === rDMA_WAIT
                    when(dmaDataW.fire()){
                        idx := idx + 1.U
                        when(leftBytes <= 8.U){
                            DMA_state := rDMA_IDLE
                            canResp := true.B
                        }  
                    }                      
                }       
                // end: DMA 写数据

            }
            is(sCACHE){                            
                val hitSignal = Wire(Bool())
                hitSignal := false.B 
                val hitID = RegInit(0.U(log2Ceil(nblock).W))
                val oneCycle = RegInit(false.B)
                val dmaDone = Wire(Bool())
                dmaDone := false.B  
                val overlapBlocks = RegInit(VecInit(Seq.fill(nblock)(false.B))) // miss 时需要写回的block              
                cstate := MuxCase(cstate, Seq(
                    (cstate===cIDLE)                -> cQUERY,                    
                    (cstate===cQUERY && !hitSignal) -> cDMA,
                    ((cstate===cQUERY && hitSignal) || 
                    (cstate === cDMA && dmaDone))   -> cRESP,  // 查询 1 cycle完成
                    (cstate===cRESP && canResp)     ->cIDLE    // banks 与 vregs间传递数据
                ))
                switch(cstate){                    
                    is(cIDLE){
                        oneCycle    := true.B 
                        // printf("@SPM: before scratchpad access\n")
                        // for(i <- 0 until nblock){
                        //     printf("%d \tv%d \td%d \taddr%x \tlgElems%d \tstride%d\n",
                        //         i.U, tabValid(i), tabDirty(i), table(i).addr, table(i).lgElem, table(i).stride)
                        // }
                        overlapBlocks := VecInit(Seq.fill(nblock)(false.B))
                    }
                    is(cQUERY){
                        // 假设数据都是bank对齐                        
                        val lm = addr 
                        val hm = lm + (stride << lgElem)*elems - ((stride-1.U) << lgElem)                        
                        for(i <- 0 until nblock){   
                            val t = table(i)                         
                            val lt = t.addr
                            val ht = t.addr + (t.stride << log2Ceil(blockBytes).U) - ((t.stride-1.U) << t.lgElem)
                            val hmb = lm + (stride << lgElem)*(blockBytes.U >> lgElem) - ((stride-1.U) << lgElem)
                            val include = (lt <= lm) && (hm <= ht)                                  // 请求地址包含在used table
                            val nonInclude = (hmb <= lt) || (lm >= ht)                                 
                            val firstDataExit = include && (((lm - lt) % (stride << lgElem)) === 0.U)// SPM中含有请求的第一个元素
                            // printf("@SPM: lt=%x, ht=%x, lm=%x, hm=%x, hmb=%x\n", lt, ht, lm, hm, hmb)    
                                 
                            
                            // 命中
                            val necessary = tabValid(i) && firstDataExit  // 必要条件                                
                            
                            val hitCondition1 = necessary && stride === t.stride && lgElem === t.lgElem
                            val hitCondition2 = necessary && 
                                ((stride % t.stride === 0.U && lgElem === t.lgElem) ||  // 类型必须相同
                                (stride === 1.U && t.stride === 1.U))                   // 类型可以不同

                            when(hitCondition2){
                                hitSignal   := true.B
                                hitID       := i.U
                                oneCycle    := hitCondition1    // 命中 && 条件1成立，则可以一次性读取
                            }
                            // 有重叠
                            val overlapDat = (Mux(lm>lt, lm-lt, lt-lm)) % (stride << lgElem) === 0.U
                            val overlap = !nonInclude &&                                        // 块间有交叉
                                !(lgElem === t.lgElem && stride === t.stride && !overlapDat)     // 排除相邻两列的情况
                            overlapBlocks(i) := tabValid(i) && (!hitCondition2) && overlap
                            
                            // printf("@SPM: i=%d, overlapBlocks=%d, overlap=%d, nonInclude=%d, overlapDat=%d\n", 
                            //     i.U, tabValid(i) && (!hitCondition2) && overlap, overlap, nonInclude, overlapDat)
                        }                        
                    }
                    is(cDMA){// miss分两种请求处理 1. overlap 2. non-overlap 
                        // non-valid > non-dirty > dirty                       
                        val randBankID = Mux(tabValid.asUInt.andR, 
                            randNum(log2Ceil(nblock)-1, 0),           
                            PriorityEncoder(~(tabValid.asUInt)))     
                        val writeBack = Wire(Bool())
                        writeBack := (overlapBlocks.asUInt.orR || tabDirty.asUInt.andR).asBool
                        val cBlockID = RegInit(0.U(wnbank.W))
                        val writeBackDone = Wire(Bool())
                        writeBackDone := false.B
                        dstate := MuxCase(dstate, Seq(
                            (dstate===dIDLE && writeBack)           -> dWBACK,  // 将脏block写回
                            ((dstate===dIDLE && !writeBack) ||                  // 不需要写回
                            (dstate===dWBACK && writeBackDone))        -> dLOAD,   // 写回结束   
                            (dstate===dLOAD && dmaDone)             -> dIDLE))  // 读取完一块 block    
                        switch(dstate){
                            is(dIDLE){
                                idx := 0.U
                                cRegsValid := false.B
                                cBlockID    := randBankID   
                                writeBack   := overlapBlocks.asUInt.orR || 
                                                tabDirty.asUInt.andR    ||
                                                (tabValid(randBankID) && tabDirty(randBankID))
                            }
                            is(dWBACK){// DMA 写回 block
                                val writeBackID = Mux(overlapBlocks.asUInt.orR, PriorityEncoder(overlapBlocks.asUInt), cBlockID)
                                writeBackDone := !tabValid(writeBackID)
                                when(!tabDirty(writeBackID)){               // 数据不脏，直接无效
                                    tabValid(writeBackID)       := false.B
                                    overlapBlocks(writeBackID)  := false.B
                                }.otherwise{                                // 数据脏，需要写回
                                    switch(DMA_state){
                                        is(rDMA_IDLE){
                                            idx := 0.U
                                            dmareqio.valid          := true.B
                                            dmareqio.bits.ren       := false.B
                                            dmareqio.bits.baseAddr  := table(writeBackID).addr
                                            dmareqio.bits.lgElem    := table(writeBackID).lgElem
                                            dmareqio.bits.elems     := blockBytes.U >> table(writeBackID).lgElem   
                                            dmareqio.bits.stride    := table(writeBackID).stride
                                            dmareqio.bits.vmask     := (~(0.U(nbank.W))).asTypeOf(Vec(nbank, Bool()))
                                            DMA_state               := Mux(dmareqio.fire(), rDMA_WAIT, rDMA_IDLE) 
                                        }
                                        is(rDMA_WAIT){
                                            // 从multi-banks读数                                                           
                                            banks_io.zipWithIndex.foreach{ case(bio, i) =>
                                                bio.read.req.bits.addr := (idx >> wnbank.U) + blockLine.U * writeBackID     // 分发地址到所有bank读端口
                                                bio.read.req.valid :=  !cRegsValid
                                                bio.read.resp.ready := true.B
                                                when(bio.read.resp.fire()){
                                                    cRegs(i) := bio.read.resp.bits.data
                                                    cRegsValid := true.B
                                                }                                    
                                            }
                                            dmaDataW.valid  := cRegsValid
                                            dmaDataW.bits   := cRegs(idx)
                                            when(dmaDataW.fire()){
                                                val nextIdx = idx + 1.U
                                                idx := nextIdx
                                                when(nextIdx === blockElems.U){     // DMA 写回block完成
                                                    DMA_state               := rDMA_IDLE
                                                    cRegsValid              := false.B  
                                                    idx                     := 0.U 
                                                    tabValid(writeBackID)       := false.B  // 修改block used table
                                                    tabDirty(writeBackID)       := false.B
                                                    overlapBlocks(writeBackID)  := false.B
                                                }  
                                                when(nextIdx(wnbank-1, 0) === 0.U){
                                                    cRegsValid := false.B
                                                    // 打印每次写回的数据
                                                    printf("@SPM: DMA write back data\n")
                                                    for(i <- 0 until nbank){
                                                        printf("%x\t", vregs(i))
                                                        if((i+1)%8 == 0){
                                                            printf("\n")
                                                        }
                                                    }
                                                }
                                            } // dmaDataW.fire end
                                        }
                                    } // switch(DMA_state)
                                }// otherwise tabDirty(writeBackID)
                            }// dWBACK end
                            is(dLOAD){  // DMA 读取block数据
                                switch(DMA_state){
                                    is(rDMA_IDLE){
                                        idx := 0.U
                                        dmareqio.valid          := true.B
                                        dmareqio.bits.ren       := true.B
                                        dmareqio.bits.baseAddr  := addr
                                        dmareqio.bits.lgElem    := lgElem
                                        dmareqio.bits.elems     := blockBytes.U >> lgElem   
                                        dmareqio.bits.stride    := stride                
                                        DMA_state               := Mux(dmareqio.fire(), rDMA_WAIT, rDMA_IDLE)                               
                                    }
                                    is(rDMA_WAIT){
                                        dmaDataR.ready := true.B 
                                        when(dmaDataR.fire()){
                                            val nextIdx = idx + 1.U
                                            val i       = idx(wnbank-1, 0)
                                            idx         := nextIdx
                                            cRegs(i)    := dmaDataR.bits
                                            when(nextIdx === blockElems.U){
                                                DMA_state := rDMA_IDLE
                                                idx := 0.U
                                                tabValid(cBlockID)      := true.B
                                                tabDirty(cBlockID)      := false.B
                                                table(cBlockID).addr    := addr
                                                table(cBlockID).lgElem  := lgElem
                                                table(cBlockID).stride  := stride
                                                dmaDone                 := true.B
                                                hitID                   := cBlockID // miss block已经加载进banks, 更新hitID
                                            }                                           
                                            
                                            when(nextIdx(wnbank-1, 0) === 0.U){// 每完成一次line读取，写回数据
                                                // 数据写回 banks
                                                val lastDat = dmaDataR.bits
                                                banks_io.zipWithIndex.foreach{ case(bio, i) =>
                                                    bio.write.mask := (~(0.U(bankElemBytes.W))).asTypeOf(Vec(bankElemBytes, Bool()))    // 全部写回
                                                    bio.write.data := (if(i<nbank-1) cRegs(i) else lastDat)
                                                    bio.write.addr := (idx >> wnbank.U) + blockLine.U * cBlockID                        // 读完64个数，写一条line，所以需要-1                
                                                    bio.write.en   := true.B
                                                    // printf("@write.addr: addr=%d, cBlockID=%d\n", bio.write.addr, cBlockID)
                                                }

                                                printf("@SPM: DMA load a block data...\n")
                                                for(i <- 0 until nbank){
                                                    if(i < nbank - 1){
                                                        printf("%x\t", cRegs(i))
                                                    }else{
                                                        printf("%x\t", lastDat)
                                                    }                                                
                                                    if((i+1)%8 == 0){
                                                        printf("\n")
                                                    }
                                                } 
                                            }// end of when(nextIdx(wnbank-1, 0) === nbank.U)
                                        }// end of when(dmaDataR.fire())
                                        
                                    }// end of is(rDMA_WAIT)
                                }// end of switch(DMA_state)                                                             
                            }// dLoad end
                        }// dstate end   
                    }// cDMA end
                    is(cRESP){// 加速器读写响应返回
                        // 假设读取的数据对齐wbankElem!!!                        
                        val bankAddrInc     = stride << (wbankElemBytes.U)  // 两个bank间数据地址增量
                        val bankDistance    = (addr - table(hitID).addr) / bankAddrInc
                        val bankStartID     = bankDistance(wnbank-1, 0)
                        val shift           = nbank.U - bankStartID        // 右移shift 位可以恢复成0->nbank顺序
                        val bankAddr        = (bankDistance >> wnbank.U) + blockLine.U*hitID
                        printf("@SPM: hitID=%d,bankAddrInc=%d, bankDistance=%d, bankStartID=%d, bankAddr=%d\n",
                         hitID, bankAddrInc, bankDistance, bankStartID, bankAddr)
                        
                        network.io.shift    := bankStartID  // 写回数据的时候需要右移，将0放到bankStartID位置
                        network.io.in       := vregs                       

                        when(oneCycle){// 数据可以一次性读取
                            banks_io.zipWithIndex.foreach{ case(bio, i) =>
                                val ibankAddr = Mux(i.U >= bankStartID, bankAddr, bankAddr + 1.U) 
                                val ii = (i.U - bankStartID + nbank.U)(wnbank-1, 0)       // i->ii映射，对应bank(i)->vregs(ii)映射
                                // 读取数据
                                bio.read.req.bits.addr := ibankAddr
                                bio.read.req.valid := ren
                                bio.read.resp.ready := true.B
                                when(bio.read.resp.fire()){ 
                                    network.io.shift    := shift
                                    network.io.in(i)    := bio.read.resp.bits.data
                                    vregs               := network.io.out                                     
                                    canResp             := true.B
                                    // printf("@cRESP: read data %d\n", bio.read.resp.bits.data)
                                }
                                // 写数据                            
                                val ii_mask = ii << (3.U-lgElem)    // 每个bank中包含lgElem元素个数 1<<(3.U-lgElem)
                                val wbiomask = MuxCase(0.U, Seq(     
                                    (lgElem === 0.U && ii_mask+7.U<64.U) -> Cat(vmask(ii_mask+7.U), vmask(ii_mask+6.U), vmask(ii_mask+5.U)
                                        , vmask(ii_mask+4.U), vmask(ii_mask+3.U), vmask(ii_mask+2.U), vmask(ii_mask+1.U), vmask(ii_mask)),
                                    (lgElem === 1.U && ii_mask+3.U<64.U) -> Cat(Fill(2, vmask(ii_mask+3.U)), 
                                        Fill(2, vmask(ii_mask+2.U)), Fill(2, vmask(ii_mask+1.U)), Fill(2, vmask(ii_mask))),
                                    (lgElem === 2.U && ii_mask+1.U<64.U) -> Cat(Fill(4, vmask(ii_mask+1.U)),Fill(4, vmask(ii_mask))),
                                    (lgElem === 3.U && ii_mask<64.U) -> Fill(8, vmask(ii_mask))
                                ))                                                                
                                bio.write.en    := !ren
                                bio.write.addr  := ibankAddr
                                bio.write.mask  := wbiomask.asTypeOf(Vec(bankElemBytes, Bool()))
                                bio.write.data  := network.io.out(i)
                            }
                            when(!ren){  // 写回只需一个时钟周期
                                canResp         := true.B
                                tabValid(hitID) := true.B
                                tabDirty(hitID) := true.B
                            }
                        }

                        when(!oneCycle){// 一个个读写bank                         
    
                        }    


                    }// cRESP end
                }// cCACHE end
            }
            is(sRESP){
                //start: 读响应k  := wbiomask.asTypeOf(Vec(bankElemBytes, Bool()))    
                io.read.resp.valid := ren===1.U    
                when(lgElem === 0.U){
                    for(i <- 0 until nbank){
                        val typeBits = 8
                        val bitsIdx = i * typeBits          
                        val k = bitsIdx / wbankElem
                        val l = bitsIdx % wbankElem
                        io.read.resp.bits.vregs(i) := elemMask(lgElem) & (vregs(k)(l+typeBits-1, l))
                    }
                }.elsewhen(lgElem === 1.U){
                    for(i <- 0 until nbank){
                        val typeBits = 16
                        val bitsIdx = i * typeBits        
                        val k = bitsIdx / wbankElem
                        val l = bitsIdx % wbankElem
                        io.read.resp.bits.vregs(i) := elemMask(lgElem) & (vregs(k)(l+typeBits-1, l))
                    }
                }.elsewhen(lgElem === 2.U){
                    for(i <- 0 until nbank){
                        val typeBits = 32
                        val bitsIdx = i * typeBits           
                        val k = bitsIdx / wbankElem
                        val l = bitsIdx % wbankElem
                        val dat = elemMask(lgElem) & (vregs(k)(l+typeBits-1, l))
                        io.read.resp.bits.vregs(i) := Mux(i.U < elems, dat, 0.U)
                    }
                }.otherwise{     
                    for(i <- 0 until nbank){
                        io.read.resp.bits.vregs(i) := Mux(i.U < elems, vregs(i), 0.U)
                    }
                                     
                }  
                //end: 读响应
                    
                //start: 写响应
                io.write.ready := true.B 
                // end: 写响应

                done := (ren===1.U && io.read.resp.fire()) || (ren===0.U)

                printf("@SPM: after scratchpad access\n")
                for(i <- 0 until nblock){
                    printf("%d \tv%d \td%d \taddr%x \tlgElems%d \tstride%d\n",
                        i.U, tabValid(i), tabDirty(i), table(i).addr, table(i).lgElem, table(i).stride)
                }
            } //sRESP end
            is(sFLUSH){
                // todo 充分考虑tabValid问题
                val hasDirtyData = tabDirty.asUInt.orR 
                val dirtyID = PriorityEncoder(tabDirty.asUInt)  // 脏数据block id
                val raddr = RegInit(0.U(log2Ceil(bankElems).W))

                dmareqio.bits.ren := false.B 
                dmareqio.bits.baseAddr  := table(dirtyID).addr
                dmareqio.bits.lgElem    := table(dirtyID).lgElem
                dmareqio.bits.elems     := blockBytes.U >> table(dirtyID).lgElem    // block 包含 lgElem 类型数据个数
                dmareqio.bits.stride    := table(dirtyID).stride
                dmareqio.bits.vmask     := (~(0.U(nbank.W))).asTypeOf(Vec(nbank, Bool()))
                
                io.flush.ready := hasDirtyData === 0.U 

                switch(fstate){
                    is(fIDLE){
                        idx := 0.U
                        raddr := dirtyID * blockLine.U                        
                        flushDone := (hasDirtyData === 0.U)
                        fstate := Mux(hasDirtyData, fWBLOCK, fIDLE)  
                        vregsValid := false.B
                    }
                    is(fWBLOCK){
                        // 发送 dma 请求
                        dmareqio.valid := DMA_state === rDMA_IDLE 
                        when(dmareqio.fire()){
                            DMA_state := rDMA_WAIT
                        }
                        // 读取SPM数据
                        banks_io.zipWithIndex.foreach{ case(bio, i) =>
                            bio.read.req.bits.addr := raddr
                            bio.read.req.valid := !vregsValid   // TODO 每条line会读两次
                            bio.read.resp.ready := true.B 
                            when(bio.read.resp.fire()){
                                vregs(i) := bio.read.resp.bits.data
                                vregsValid := true.B                         
                            }
                        }
                        // dma 写回
                        dmaDataW.valid := DMA_state === rDMA_WAIT && vregsValid
                        dmaDataW.bits := vregs(idx(wnbank-1, 0))
                        when(dmaDataW.fire()){
                            val nextIdx = idx + 1.U
                            idx := nextIdx 
                            when(nextIdx === blockElems.U){
                                DMA_state := rDMA_IDLE
                                fstate    := fIDLE
                                tabDirty(dirtyID) := false.B 
                                //tabValid(dirtyID) := false.B 
                            }   
                            // 读取下条 line 
                            when(nextIdx(wnbank-1, 0) === 0.U){
                                vregsValid := false.B
                                raddr := raddr + 1.U 
                            }
                        }
                    }// fWBLOCK end
                }// fstate end
            }// sFLUSH end
        }
        // printf("@SPM: state=%d, cstate=%d, dstate=%d, fstate=%d, DMA_state=%d, leftBytes=%d, idx=%d\n", 
        //     state, cstate, dstate, fstate, DMA_state, leftBytes, idx)
        
    }
    
}
