package memaccer
import chisel3._
import chisel3.util._

object ScratchpadConfig{
    //eg: sramBytes=16KB 、 nblock=8时, blockLine=4
    val sramBytes = 32*1024                               // SPM缓存大小，单位byte
    val nblock = 32      
    val nbank = 64                                          // multi-banks bank个数需要根据PE个数做出调整
    val wbankElem = 64    
    val blockBytes = sramBytes / nblock                     // 替换更新SPM块大小    
    val wnbank = log2Ceil(nbank)    
    val bankElemBytes = wbankElem >> 3
    val wbankElemBytes = log2Ceil(bankElemBytes)
    val bankElems = sramBytes / (nbank * bankElemBytes)     // bank包含元素个数
    val blockLine = bankElems / nblock                      // block 包含元素向量个数
    val blockElems = nbank * blockLine                      // block 包含元素个数
}