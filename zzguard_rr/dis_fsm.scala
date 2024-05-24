// package freechips.rocketchip.zzguardrr

// import chisel3._
// import chisel3.util._

// class dis_fsm extends Module{
//   val io = IO(new Bundle{
//     val sel       =   Input(UInt(2.W))
//     val num       =   Output(UInt(3.W))
//   })
//   //sel=1 rr fifo0-2, sel=2 block fifo3-5  
//   val rr    = Module(new fsm_rr)
//   val block = Module(new fsm_block)

//   when(io.sel === 1.U){
//     rr.io.en    := true.B
//     block.io.en := false.B
//     io.num      := rr.io.num
//   }
//   .elsewhen(io.sel === 2.U){
//     rr.io.en := false.B
//     block.io.en := true.B
//     io.num      := block.io.num
//   }
//   .otherwise{
//     rr.io.en := false.B
//     block.io.en := false.B
//     io.num      := 0.U
//   }


// }

