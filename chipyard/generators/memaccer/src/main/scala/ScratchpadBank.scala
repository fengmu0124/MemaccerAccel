package memaccer

import chisel3._
import chisel3.util._

// scratchpadBank 实现参考gemmini
class ScratchpadReadReq(val n: Int) extends Bundle{
    val addr = UInt(log2Ceil(n).W)// 读请求地址
}// 读请求

class ScratchpadReadResp(val w: Int) extends Bundle{
    val data = UInt(w.W)// 读响应数据
}// 读响应

class ScratchpadReadIO(val n: Int, val w: Int) extends Bundle {
  val req = Decoupled(new ScratchpadReadReq(n))// Decoupled是一个bundle，包含valid和bits两个字段,读请求
  val resp = Flipped(Decoupled(new ScratchpadReadResp(w)))// 读响应
}// 读IO

class ScratchpadWriteIO(val n: Int, val w: Int, val mask_len: Int) extends Bundle {
  val en = Output(Bool())// 写使能
  val addr = Output(UInt(log2Ceil(n).W))// 写地址
  val mask = Output(Vec(mask_len, Bool()))// 写掩码
  val data = Output(UInt(w.W))// 写数据
}// 写IO


class ScratchpadBank(n: Int, w: Int) extends Module{
    val mask_len = w / 8                    // bank elem掩码位数
    val mask_elem = UInt(8.W)               // 掩码的每一位，控制8bits
    val io = IO(new Bundle{
        val read = Flipped(new ScratchpadReadIO(n, w))// 读IO
        val write = Flipped(new ScratchpadWriteIO(n, w, mask_len))// 写IO
    })//Flipped表示反转，即输入变输出，输出变输入

    val mem = SyncReadMem(n, Vec(mask_len, mask_elem))// 读写内存

    when(io.write.en){
        mem.write(io.write.addr, io.write.data.asTypeOf(Vec(mask_len, mask_elem)), io.write.mask)// 写内存
        printf("@BANK(write): addr=%x, data=%x, mask=%x\n", io.write.addr, io.write.data, io.write.mask.asUInt)// 打印写信息
    }// 写内存

    // read respone 后才能撤销 req.valid
    val raddr = io.read.req.bits.addr// 读地址
    io.read.req.ready := true.B// 读请求准备好
    val ren = io.read.req.fire() // 读请求发出
    val rdata = mem.read(raddr, ren).asUInt()// 读数据
  
    io.read.resp.valid := RegNext(ren)  // SyncReadMem 读需要额外一个时钟周期延迟
    io.read.resp.bits.data := rdata// 读数据
    when(io.read.resp.fire()){
        printf("@BANK(read): addr=%x, data=%x\n", raddr, rdata)
    }    // 打印读信息
 
}
