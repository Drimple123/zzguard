package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class FifoIO[T <: Data](private val gen: T) extends Bundle {
    val enq = Flipped(new DecoupledIO(gen))
    val deq = new DecoupledIO(gen)
}

abstract class Fifo[T <: Data](gen: T, val depth: Int) extends Module {
    val io = IO(new FifoIO(gen))
}

class RegFifo[T <: Data](gen: T, depth: Int) extends Fifo(gen: T, depth: Int) {
    def counter(depth: Int, incr: Bool): (UInt, UInt) = {
        val cntReg = RegInit(0.U(log2Ceil(depth).W))
        val nextVal = Mux(cntReg === (depth-1).U, 0.U, cntReg + 1.U)
        when (incr) {
            cntReg := nextVal
        }
        (cntReg, nextVal)
    }
    //信号被优化了一部分，用下dontTouch
    dontTouch(io.enq)
    dontTouch(io.deq)
    // 基于寄存器的内存
    val memReg = Reg(Vec(depth, gen))

    val incrRead = WireDefault(false.B)
    val incrWrite = WireDefault(false.B)
    val (readPtr, nextRead) = counter(depth, incrRead)
    val (writePtr, nextWrite) = counter(depth, incrWrite)

    val emptyReg = RegInit(true.B)
    val fullReg = RegInit(false.B)

    when (io.enq.valid && !fullReg) {
        memReg(writePtr) := io.enq.bits
        emptyReg := false.B
        fullReg := nextWrite === readPtr
        incrWrite := true.B
    }

    when (io.deq.ready && !emptyReg) {
        fullReg := false.B
        emptyReg := nextRead === writePtr
        incrRead := true.B
    }
    
    val readPtr_r = RegInit(0.U)
    readPtr_r := readPtr
    io.deq.bits := memReg(readPtr_r)
    io.enq.ready := !fullReg
    io.deq.valid := !emptyReg
}



