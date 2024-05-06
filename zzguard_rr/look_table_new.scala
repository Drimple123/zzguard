package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class look_table1(width: Int) extends Module{
  val io = IO(new Bundle{
    val opcode    =   Input(UInt(7.W))
    val data_in   =   Input(UInt(width.W))
    val addr      =   Input(UInt(7.W))
    val ren       =   Input(Bool())
    val wen       =   Input(Bool())
    val bitmap    =   Output(UInt(width.W))
  })

  val table = SyncReadMem(128,UInt(width.W))
  val bitmap_w = WireDefault(0.U(width.W))
  bitmap_w := table.read(io.opcode,io.ren)
  io.bitmap := bitmap_w
  when(io.wen === true.B){
    table.write(io.addr,io.data_in)
  }
}

class look_table2(depth: Int) extends Module{
  val io = IO(new Bundle{
    val bitmap    =   Input(UInt(depth.W))
    val data_in   =   Input(UInt(4.W))
    val addr      =   Input(UInt(log2Ceil(depth).W))
    val ren       =   Input(Bool())
    val wen       =   Input(Bool())
    val sel       =   Output(UInt(4.W))
  })

  val table = Mem(depth,UInt(4.W))
  //创建一个数组，保存每次访问的结果，最后进行或操作
  //val selv = Wire(Vec(depth, UInt(2.W)))
  val selv = VecInit(0.U(4.W), 0.U(4.W), 0.U(4.W), 0.U(4.W))
  when(io.wen === true.B){
    table.write(io.addr,io.data_in)
  }
  //根据bitmap来访问第二张表
  when(io.ren === true.B){
    for(i <- 0 to depth-1){
      when(io.bitmap(i) === 1.U){
        selv(i) := table(i)
      }
      .otherwise{
        selv(i) := 0.U
      }
    }
  }
  //参数化失败，不知道怎么用for把depth个结果或在一起
  io.sel := selv(0) | selv(1) | selv(2) | selv(3)
}


class look_2table_ram(width: Int) extends Module {
  val io = IO(new Bundle {
    val opcode    = Input(UInt(7.W))
    val sel       = Output(UInt(4.W))
    val bitmap    = Output(UInt(width.W))
    
    val ren1      = Input(Bool())
    val ren2      = Input(Bool())

    val wen1      = Input(Bool())
    val addr1     = Input(UInt(7.W))
    val data_in1  = Input(UInt(width.W))
    val wen2      = Input(Bool())
    val addr2     = Input(UInt(log2Ceil(width).W))
    val data_in2  = Input(UInt(4.W))

  })
  
  val table1 = Module(new look_table1(width))
  val table2 = Module(new look_table2(width))

  table1.io.ren := io.ren1
  table2.io.ren := io.ren2

  table1.io.wen     := io.wen1
  table1.io.addr    := io.addr1
  table1.io.data_in := io.data_in1
  table2.io.wen     := io.wen2
  table2.io.addr    := io.addr2
  table2.io.data_in := io.data_in2

  table1.io.opcode  := io.opcode
  table2.io.bitmap  := table1.io.bitmap
  io.sel            := table2.io.sel

  io.bitmap := table1.io.bitmap
}




