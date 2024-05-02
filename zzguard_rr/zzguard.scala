// package freechips.rocketchip.zzguardrr

// import chisel3._
// import chisel3.util._
// import freechips.rocketchip.tile.{ClockDividerN, clkdiv2_zz, clkdiv4_zz}


// class zzguardrr extends Module{
//   val io = IO(new Bundle{
//     //val addr        =   Input(UInt(2.W))
//     val valid       =   Input(Bool())
//     val din_pc      =   Input(UInt(40.W))
//     val din_ins     =   Input(UInt(32.W))
//     val din_wdata   =   Input(UInt(64.W))
//     val din_mdata   =   Input(UInt(64.W))
//     //val full_counter=   Output(Bool())
//     val yaofull_counter = Output(Bool())
//   })
  
//   dontTouch(io)
//   //因为查表控制信号慢了1拍，所以数据也慢1拍
//   val ins_r   = RegNext(io.din_ins,0.U)
//   val wdata_r = RegNext(io.din_wdata,0.U)
//   val mdata_r = RegNext(io.din_mdata,0.U)
//   val valid_r = RegNext(io.valid,false.B)

//   val table = Module(new look_2table)
//   table.io.opcode    := io.din_ins(6,0)

//   val bitmap = WireDefault(0.U(2.W))
//   bitmap := table.io.bitmap

//   val cat = Module(new instruction_cat1)
//   //cat.io.in_1  := io.din_pc
//   cat.io.ins    := ins_r
//   cat.io.wdata  := wdata_r
//   cat.io.mdata  := mdata_r
//   cat.io.sel    := table.io.sel
  
//   // val dis = Module(new dis_fsm)
//   // dis.io.sel := table.io.data_out
//   //io.num  := dis.io.num

//   // val en_valid = WireDefault(false.B)
//   // en_valid := MuxCase(false.B, Array(
//   //    (table.io.data_out === 0.U) -> false.B,
//   //    (table.io.data_out === 1.U && io.valid) -> true.B,
//   //    (table.io.data_out === 2.U && io.valid) -> true.B,
//   //    (table.io.data_out === 3.U) -> false.B
//   //  ))
  

//   //val num_r = RegInit(0.U(3.W))
//   //num_r := dis.io.num
//   //例化6个fifo
//   //val fifo = VecInit(Seq.fill(2)(Module(new RegFifo(UInt(160.W),3)).io))
//   //例化6个zzz
//   //val zz = VecInit(Seq.fill(6)(Module(new zzz).io))

//   // for(i <- 0 to 5){
//   //   //fifo的deq端接zzz
//   //   fifo(i).deq.ready   := zz(i).ready
//   //   zz(i).valid         := fifo(i).deq.valid
//   //   zz(i).din           := fifo(i).deq.bits

//   //   //instruction_cat接fifo的enq端
//   //   fifo(i).enq.bits  := cat.io.out
//   //   cat.io.ready      := fifo(i).enq.ready
    

//   //   //num对应的fifo才valid
//   //   when(i.asUInt === dis.io.num && en_valid){
//   //     fifo(i).enq.valid := true.B
//   //   }
//   //   .otherwise{
//   //     fifo(i).enq.valid := false.B
//   //   }

//   // }
//   // val clkdiv_zz = Module(new clkdiv4_zz)
//   // clkdiv_zz.io.clk_in := clock
//   // val clkzz_out = Wire(Clock())
//   // clkzz_out := clkdiv_zz.io.clk_out
//   // dontTouch(clkzz_out)

//   val clk_div = Module(new clkdiv4_zz)
//   clk_div.io.clk_in := clock

//   val q = VecInit(Seq.fill(2)(Module(new asyncfifo(16, 160)).io))
  
//   //看counter的fifo的num的积累情况
//   val num_num = RegInit(VecInit(Seq.fill(15)(0.U(12.W))))
//   dontTouch(num_num)
//   for(i <- 1 to 14){
//     when(q(1).wen && (q(1).num_out === i.U)){
//       num_num(i) := num_num(i) + 1.U
//     }
//   }

//   //fifo的enq端接cat,valid由bitmap决定
//   for(i <- 0 to 1){

//     q(i).clk_r := clk_div.io.clk_out

//     q(i).wdata     := cat.io.out
//     //cat.io.ready   := !(q(i).full)  //ready的接法不对，要改，感觉不能两个接一个
//     when(valid_r){
//       when(bitmap(i) === 1.U){
//         q(i).wen := true.B
//       }
//       .otherwise{
//         q(i).wen := false.B
//       }
//     }
//     .otherwise{
//       q(i).wen := false.B
//     }
//   }
  

//   val ss      = Module(new shadow_stack)
//   val counter = Module(new counter_sl)

//   //io.full_counter := q(1).full
//   io.yaofull_counter := q(1).yaofull

//   ss.clock := clk_div.io.clk_out
//   counter.clock := clk_div.io.clk_out

//   q(0).ren   := ss.io.ready
//   ss.io.valid         := !(q(0).empty)
//   ss.io.din           := q(0).rdata

//   q(1).ren   := counter.io.ready
//   counter.io.valid    := !(q(1).empty)
//   counter.io.din      := q(1).rdata
  





// }

