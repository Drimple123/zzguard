// package freechips.rocketchip.zzguardrr

// import chisel3._
// import chisel3.util._

// class look_table1 extends Module{
//   val io = IO(new Bundle{
//     val opcode    =   Input(UInt(7.W))
//     val data_in   =   Input(UInt(2.W))
//     val addr      =   Input(UInt(7.W))
//     val ren       =   Input(Bool())
//     val wen       =   Input(Bool())
//     val bitmap    =   Output(UInt(2.W))
//   })

//   val table = SyncReadMem(128,UInt(2.W))

//   io.bitmap := table.read(io.opcode,io.ren)
//   when(io.wen === true.B){
//     table.write(io.addr,io.data_in)
//   }
// }

// class look_table2 extends Module{
//   val io = IO(new Bundle{
//     val bitmap    =   Input(UInt(2.W))
//     val data_in   =   Input(UInt(2.W))
//     val addr      =   Input(UInt(1.W))
//     val ren       =   Input(Bool())
//     val wen       =   Input(Bool())
//     val sel       =   Output(UInt(2.W))
//   })

//   val table = Mem(2,UInt(2.W))
//   val selv = VecInit(0.U(2.W), 0.U(2.W))
//   for(i <- 0 to 1){
//     selv(1)
//   }

//   // io.data_out := table.read(io.addr,io.ren)
//   when(io.wen === true.B){
//     table.write(io.addr,io.data_in)
//   }
//   when(io.ren === true.B){
//     for(i <- 0 to 1){
//       when(io.bitmap(i) === 1.U){
//         selv(i) := table(i)
//       }
//       .otherwise{
//         selv(i) := 0.U
//       }
//     }
//   }
//   io.sel := selv(0) | selv(1)
// }


// class look_2table_ram extends Module {
//   val io = IO(new Bundle {
//     val opcode    = Input(UInt(7.W))
//     val sel       = Output(UInt(2.W))
//     val bitmap    = Output(UInt(2.W))

//     val wen1      = Input(Bool())
//     val addr1     = Input(UInt(7.W))
//     val data_in1  = Input(UInt(2.W))
//     val wen2      = Input(Bool())
//     val addr2     = Input(UInt(1.W))
//     val data_in2  = Input(UInt(2.W))

//   })
  
//   val table1 = Module(new look_table1)
//   val table2 = Module(new look_table2)

//   table1.io.ren := true.B
//   table2.io.ren := true.B

//   table1.io.wen     := io.wen1
//   table1.io.addr    := io.addr1
//   table1.io.data_in := io.data_in1
//   table2.io.wen     := io.wen2
//   table2.io.addr    := io.addr2
//   table2.io.data_in := io.data_in2

//   table1.io.opcode  := io.opcode
//   table2.io.bitmap  := table1.io.bitmap
//   io.sel            := table2.io.sel

//   io.bitmap := table1.io.bitmap
// }




