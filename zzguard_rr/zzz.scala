package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class zzz extends Module{
  val io = IO(new Bundle{
    val valid = Input(Bool())
    val din   = Input(UInt(104.W))
    val ready = Output(Bool())
    val dout  = Output(UInt(104.W))
  })
  //信号被优化，用下dontTouch
    dontTouch(io)

    io.ready := true.B

    val dout_r = RegInit(0.U(104.W))

    dout_r := io.din
    
    io.dout := dout_r

}

