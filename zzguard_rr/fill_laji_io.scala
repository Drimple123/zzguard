package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class fill_laji_io(width: Int) extends Module{
  val io = IO(new Bundle{
    val deq = Decoupled(UInt(width.W))
  })
  io.deq.valid := false.B
  io.deq.bits := 0.U

}

