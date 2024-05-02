package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._
import os.write

class asan extends Module{
  val io = IO(new Bundle{
    
    
    
    
    
    
    
    
    
    val wr_in     = Input(Bool())
    val rd_in     = Input(Bool())
    val addr_wr   = Input(UInt(40.W))
    val addr_rd   = Input(UInt(40.W))
    val size      = Input(UInt(8.W))    

    val wr_out    = Output(Bool())
    val rd_out    = Output(Bool())
    val addr_out  = Output(UInt(40.W))
    val data_out  = Output(UInt(8.W))

    val can_use   = Output(Bool())
    val uaf       = Output(Bool())
    val overflow  = Output(Bool())

    val data_in   = Input(UInt(8.W))

    val ready_fifo= Output(Bool())
    val valid_fifo= Input(Bool())

    val valid_ro  = Input(Bool())

    val valid_mem = Input(Bool())
  })

  val can_use_r = RegInit(true.B)
  val uaf_r     = RegInit(false.B)
  val overflow_r= RegInit(false.B)

  val addr_in_r = RegInit(0.U(40.W))
  //将地址存一下，和访问shadow mem的结果比较
  when(io.valid_fifo){
    addr_in_r := io.addr_rd
  }
     

  val ready_fifo_r = RegInit(false.B)

  val addr_out_w = WireDefault(0.U(40.W))
  when(io.wr_in){
    addr_out_w := io.addr_wr >> 5.U + "h1_0000_0000".U
  }
  .elsewhen(io.rd_in){
    addr_out_w := io.addr_rd >> 5.U + "h1_0000_0000".U
  }

  io.addr_out := addr_out_w
  //数据来了，要访存，先把ready_fifo拉低
  when(io.valid_fifo){
    ready_fifo_r := false.B
  }
  io.ready_fifo := ready_fifo_r
  //malloc or free,rocc instruction, data_in由c那边去搞, 写内存
  io.wr_out   := io.wr_in && io.valid_ro
  io.data_out := io.size

  //load or store，读内存,判断是否可用
  io.rd_out   := io.rd_in && io.valid_fifo
  when(io.valid_mem){
    ready_fifo_r := true.B    //访存数据回来了，把ready 拉高
    when(io.data_in === 255.U){
      uaf_r     := true.B
      can_use_r := false.B
      overflow_r:= false.B
    }
    .elsewhen(io.data_in === 0.U){
      uaf_r     := false.B
      can_use_r := true.B
      overflow_r:= false.B
      
    }
    .elsewhen(io.data_in >= addr_in_r(4,0)){
      uaf_r     := false.B
      can_use_r := true.B
      overflow_r:= false.B
    }
    .otherwise{
      uaf_r     := false.B
      can_use_r := false.B
      overflow_r:= true.B
    }
  }
  .otherwise{
    uaf_r     := false.B
    can_use_r := true.B
    overflow_r:= false.B
  }

  io.uaf      := uaf_r
  io.can_use  := can_use_r
  io.overflow := overflow_r

}

