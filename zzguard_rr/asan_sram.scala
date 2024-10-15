package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.ClockDividerN
//lht start
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
//lht end
class asan_sram extends Module{
  val io = IO(new Bundle{
    val din = Flipped(Decoupled(UInt(160.W)))
    val rocc_in = Flipped(Decoupled(UInt(55.W)))

  })
  //
  val mem = SyncReadMem(4096,UInt(6.W))

  val data_back = RegInit(0.U)
  val valid = RegInit(false.B)
  dontTouch(data_back)
  dontTouch(valid)

  io.din.ready := true.B
  io.rocc_in.ready := true.B

  //din decode
  val lors_addr = io.din.bits(103,64)
  val sram_addr = (lors_addr - "h80004470".U) >> 5.U
  val lors_addr_r = RegNext(lors_addr)

  //rocc decode
  val rocc_funct = io.rocc_in.bits(6,0)
  val rocc_addr = io.rocc_in.bits(54,15)
  val rocc_addr_w = (rocc_addr - "h80004470".U) >> 5.U
  val rocc_size = io.rocc_in.bits(14,7)

  dontTouch(lors_addr)
  dontTouch(sram_addr)
  dontTouch(rocc_funct)
  dontTouch(rocc_addr)
  dontTouch(rocc_size)
  //result
  val can_use_r = RegInit(true.B)
  val uaf_r     = RegInit(false.B)
  val overflow_r= RegInit(false.B)
  dontTouch(can_use_r)
  dontTouch(uaf_r)
  dontTouch(overflow_r)


  //rocc 9,first address of heap  10,malloc sram addr and size
  val f_addr = RegInit(0.U)
  when(io.rocc_in.fire){
    when(rocc_funct === 9.U){
      f_addr := rocc_addr
    }
    .elsewhen(rocc_funct === 6.U){
      mem.write(rocc_addr_w,rocc_size)
    }
  }

  //access sram
  when(io.din.fire){
    data_back := mem.read(sram_addr, true.B)
    valid := true.B
  }
  .otherwise{
    valid := false.B
  }

  //compare
  when(valid === true.B){
    when(data_back === 64.U){
      uaf_r     := true.B
      can_use_r := false.B
      overflow_r:= false.B
    }
    .elsewhen(data_back === 0.U){
      uaf_r     := false.B
      can_use_r := true.B
      overflow_r:= false.B
    }
    .elsewhen(data_back >= lors_addr_r(4,0)){
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



}

