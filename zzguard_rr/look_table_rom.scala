package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class look_table_rom extends Module {
  val io = IO(new Bundle {
    val addr      = Input(UInt(2.W))
    val data_out  = Output(UInt(2.W))
  })

  val rom = VecInit(1.U, 2.U, 0.U, 3.U)

  //val data = RegInit(0.U(2.W))
  
  //data := rom(io.addr)

  //io.data_out := data

  io.data_out := rom(io.addr)
}

// ins是jal\jalr给shadowstack out=1, ins是store\load给counter, out=2,其他out=0
class look_table1_when extends Module {
  val io = IO(new Bundle {
    val opcode      = Input(UInt(7.W))
    val bitmap      = Output(UInt(2.W))
  })

  val bitmap_reg = RegInit(0.U(2.W))
  //1101111是jal，1100111是jalr   
  when(io.opcode === "b1101111".U || io.opcode === "b1100111".U){
    bitmap_reg := 1.U
  }//0100011是store, 0000011是load   
  .elsewhen(io.opcode === "b0100011".U || io.opcode === "b0000011".U){
    bitmap_reg := 2.U
  }
  .otherwise{
    bitmap_reg := 0.U
  }

  io.bitmap := bitmap_reg

}

//两行，第一行是ss 11，第二行是counter 00
class look_table2_rom extends Module {
  val io = IO(new Bundle {
    val bitmap    = Input(UInt(2.W))
    val sel       = Output(UInt(2.W))
  })
  val selv = Wire(Vec(2,UInt(2.W)))

  val rom = VecInit(3.U, 0.U)

  for(i <- 0 to 1){
    when(io.bitmap(i) === 1.U){
      selv(i) := rom(i)
    }
    .otherwise{
      selv(i) := 0.U
    }
  }

  io.sel := selv(0) | selv(1)
}

class look_2table extends Module {
  val io = IO(new Bundle {
    val opcode    = Input(UInt(7.W))
    val sel       = Output(UInt(2.W))
    val bitmap    = Output(UInt(2.W))
  })
  
  val table1 = Module(new look_table1_when)
  val table2 = Module(new look_table2_rom)

  table1.io.opcode  := io.opcode
  table2.io.bitmap  := table1.io.bitmap
  io.sel            := table2.io.sel

  io.bitmap := table1.io.bitmap
}

