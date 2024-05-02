package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class counter_sl extends Module{
  val io = IO(new Bundle{
    val valid         = Input(Bool())
    val din           = Input(UInt(160.W))
    val ready         = Output(Bool())
    val number_store  = Output(UInt(20.W))
    val number_load   = Output(UInt(20.W))
  })
  //信号被优化，用下dontTouch
    dontTouch(io)

    io.ready := true.B

    val opcode = WireDefault(0.U(7.W))
    opcode := io.din(134,128)

    val number_store_r = RegInit(0.U(20.W))
    val number_load_r  = RegInit(0.U(20.W))

    when(io.valid){
      when(opcode === "b0100011".U){
        number_store_r := number_store_r + 1.U
      }
      .elsewhen(opcode === "b0000011".U){
        number_load_r := number_load_r + 1.U
      }
    }

    io.number_load  := number_load_r
    io.number_store := number_store_r

    // val dout_r = RegInit(0.U(104.W))

    // dout_r := io.din
    
    // io.dout := dout_r

}

class counter_losuan extends Module{
  val io = IO(new Bundle{
    // val valid         = Input(Bool())
    // val din           = Input(UInt(160.W))
    // val ready         = Output(Bool())
    val enq           = Flipped(Decoupled(UInt(160.W)))
    val number_losuan = Output(UInt(20.W))
  })
  //信号被优化，用下dontTouch
    dontTouch(io)

    io.enq.ready := true.B

    val number_losuan_r  = RegInit(0.U(20.W))

    when(io.enq.fire){
     number_losuan_r := number_losuan_r + 1.U
    }

    io.number_losuan := number_losuan_r

    // val dout_r = RegInit(0.U(104.W))

    // dout_r := io.din
    
    // io.dout := dout_r

}

