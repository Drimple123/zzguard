package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class myarbiter extends Module {
  val io = IO(new Bundle {
      val in = Flipped(Vec(3, Decoupled(UInt(8.W))))
      val out = Decoupled(UInt(8.W))
      val chosen = Output(UInt())
  })
  dontTouch(io)
  val arbiter = Module(new Arbiter(UInt(8.W), 3))  // 2 to 1 Priority Arbiter
  arbiter.io.in <> io.in
  io.out <> arbiter.io.out
  io.chosen := arbiter.io.chosen
}

class zz_RRarbiter(val n:Int) extends Module {
  val io = IO(new Bundle{
    // val req = Input(Vec(n, Bool()))
    // val gnt = Output(Vec(n, Bool()))
    val req = Input(UInt(n.W))
    val gnt = Output(UInt(n.W))
  })
  val base = RegInit(1.U(n.W))

  val double_req = Cat(io.req, io.req)

  val double_gnt = double_req & (~(double_req - base))

  io.gnt := double_gnt(n-1,0) | double_gnt(2*n-1,n)

  when(io.req.asBools.reduce(_ | _)){
    base := Cat(io.gnt(n-2,0),io.gnt(n-1))
  }


}

