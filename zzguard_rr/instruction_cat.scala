package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class instruction_cat extends Module{
  val io = IO(new Bundle{
    val in_1    =   Input(UInt(40.W))
    val in_2    =   Input(UInt(32.W))
    val in_3    =   Input(UInt(64.W))
    val sel     =   Input(UInt(2.W))
    val out     =   Output(UInt(104.W))

    val ready   =   Input(Bool())
    //val valid   =   Output(Bool())
  })
  
  val out_w = WireDefault(0.U(104.W))

  //val out_r = RegInit(0.U(4.W))

  when(io.sel === 1.U && io.ready){
    out_w := Cat(io.in_1, io.in_2)
    //out_r := Cat(io.in_1, io.in_2)
  }
  .elsewhen(io.sel === 2.U && io.ready){
    out_w := Cat(io.in_1, io.in_3)
    //out_r := Cat(io.in_1, io.in_3)
  }
  .otherwise{
    //out_w := 0.U
    //out_r := 0.U
  }

  //io.valid := true.B
  io.out   := out_w
  //io.out := out_r
  
}

class instruction_cat1 extends Module{
  val io = IO(new Bundle{
    val ins      =   Input(UInt(32.W))
    val wdata    =   Input(UInt(64.W))
    val mdata    =   Input(UInt(64.W))
    val npc      =   Input(UInt(40.W))
    val req_addr =   Input(UInt(40.W))
    val sel      =   Input(UInt(4.W))
    val out      =   Output(UInt(160.W))

    //val ready   =   Input(Bool())
    //val valid   =   Output(Bool())
  })
  
  dontTouch(io)
  val cat_1 = Wire(UInt(64.W))
  val cat_2 = Wire(UInt(64.W))
  cat_1 := MuxCase(0.U, Array(
    (io.sel(3) === 1.U) -> io.wdata,
    (io.sel(2) === 1.U) -> io.mdata
  ))
  cat_2 := MuxCase(0.U, Array(
    (io.sel(1) === 1.U) -> io.npc,
    (io.sel(0) === 1.U) -> io.req_addr
  ))
  val cat_12 = Cat(cat_1, cat_2)
  val out_w = Cat(io.ins, cat_1, cat_2)

  // val out_w   = WireDefault(0.U(160.W))
  // val cat_w   = WireDefault(0.U(64.W))
  // val cat_m   = WireDefault(0.U(64.W))
  // val cat_wm  = WireDefault(0.U(128.W))

  // cat_w  := Mux(io.sel(1) === 1.U, io.wdata, 0.U)
  // cat_m  := Mux(io.sel(0) === 1.U, io.mdata, 0.U)
  // cat_wm := Cat(cat_w, cat_m)
  // out_w  := Cat(io.ins,cat_wm)

  io.out := out_w
}

