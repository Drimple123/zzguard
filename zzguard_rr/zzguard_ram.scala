// package freechips.rocketchip.zzguardrr

// import chisel3._
// import chisel3.util._
// import freechips.rocketchip.tile.ClockDividerN
// //lht start
// import freechips.rocketchip.tile._
// import freechips.rocketchip.diplomacy._
// import org.chipsalliance.cde.config.Parameters
// //lht end
// class zzguardrr_ram(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
//   override lazy val module = new zzguardrr_ramImp (this)
// }

// class zzguardrr_ramImp(outer: zzguardrr_ram)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
//     with HasCoreParameters {
//   //val io = IO(new Bundle{
//     //val addr        =   Input(UInt(2.W))
//     val valid       =   Reg(Bool())
//     val din_pc      =   Reg(UInt(40.W))
//     val din_ins     =   Reg(UInt(32.W))
//     val din_wdata   =   Reg(UInt(64.W))
//     val din_mdata   =   Reg(UInt(64.W))

//     //传到另一个核里去
//     // val asan_io = IO(new Bundle{
//     //   val valid = Output(Bool())
//     //   val addr  = Output(UInt(40.W))
//     //   val size  = Output(UInt(40.W))
//     // })
//     // dontTouch(asan_io)
    
//   //})
  
//     val cmd                     = io.cmd
//     val funct                   = cmd.bits.inst.funct
//     val rs2                     = cmd.bits.inst.rs2
//     val rs1                     = cmd.bits.inst.rs1
//     val xd                      = cmd.bits.inst.xd
//     val xs1                     = cmd.bits.inst.xs1
//     val xs2                     = cmd.bits.inst.xs2
//     val rd                      = cmd.bits.inst.rd
//     val opcode                  = cmd.bits.inst.opcode

//     val rs1_val                 = cmd.bits.rs1
//     val rs2_val                 = cmd.bits.rs2
//     val rd_val                  = WireInit(0.U(xLen.W))
//   dontTouch(cmd)  
//   dontTouch(io)

//   // asan_io.addr := cmd.bits.rs1
//   // asan_io.size := cmd.bits.rs2

//   io.asan_addr := cmd.bits.rs1
//   io.asan_size := cmd.bits.rs2
//   io.asan_funct:= cmd.bits.inst.funct

//   cmd.ready                  := true.B
//   io.resp.bits.rd            := cmd.bits.inst.rd
//   io.resp.valid              := cmd.valid
//   //io.resp.bits.data          := counter.io.number_load
//   io.busy                    := cmd.valid
  
//   //mask,写表之前为0,写完表置1,程序运行完之后置0
//   val cfg_mask = RegInit(0.U)

//   valid       := io.valid & cfg_mask
//   din_pc      := io.pc & Fill(40, cfg_mask)
//   din_ins     := io.ins & Fill(32, cfg_mask)
//   din_wdata   := io.wdata & Fill(64, cfg_mask)
//   din_mdata   := io.mdata & Fill(64, cfg_mask)
  
//   //因为查表控制信号慢了1拍，所以数据也慢1拍
//   val ins_r   = RegNext(din_ins,0.U)
//   val wdata_r = RegNext(din_wdata,0.U)
//   val mdata_r = RegNext(din_mdata,0.U)
//   val valid_r = RegNext(valid,false.B)

//   val table = Module(new look_2table_ram)
//   table.io.opcode   := din_ins(6,0)
//   table.io.addr1    := rs1_val
//   table.io.addr2    := rs1_val
//   table.io.data_in1 := rs2_val
//   table.io.data_in2 := rs2_val

//   when(cmd.fire()){
//     when((funct === 5.U)||(funct === 6.U)){//传到另一个核的asan
//       io.asan_valid := true.B
//       //asan_io.valid := true.B
//       table.io.wen1 := false.B
//       table.io.wen2 := false.B
//     }
//     .elsewhen(funct === 4.U){//写表完成，开始检测
//       io.asan_valid := false.B
//       //asan_io.valid := false.B
//       cfg_mask := 1.U
//       table.io.wen1 := false.B
//       table.io.wen2 := false.B
//     }
//     .elsewhen(funct === 8.U){//主要程序跑完，结束检测
//       io.asan_valid := false.B
//       //asan_io.valid := false.B
//       cfg_mask := 0.U
//       table.io.wen1 := false.B
//       table.io.wen2 := false.B
//     }
//     .elsewhen(funct === 1.U){  //写第一张表
//       io.asan_valid := false.B
//       //asan_io.valid := false.B
//       table.io.wen1 := true.B
//       table.io.wen2 := false.B
//     }
//     .elsewhen(funct === 2.U){  //写第二张表
//       io.asan_valid := false.B
//       //asan_io.valid := false.B
//       table.io.wen1 := false.B
//       table.io.wen2 := true.B
//     }
//     .otherwise{
//       io.asan_valid := false.B
//       //asan_io.valid := false.B
//       table.io.wen1 := false.B
//       table.io.wen2 := false.B
//     }
//   }
//   .otherwise{
//     io.asan_valid := false.B
//     //asan_io.valid := false.B
//     table.io.wen1 := false.B
//     table.io.wen2 := false.B
//   }


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

//   val clk_div = Module(new ClockDividerN(2))
//   clk_div.io.clk_in := clock

//   val q = VecInit(Seq.fill(2)(Module(new asyncfifo(16, 160)).io))
  
//   //用于rocc输出counter_sl返回的number
//   val q_back = Module(new asyncfifo(8,20))
//   q_back.clock := clk_div.io.clk_out
//   q_back.io.clk_r := clock
  
//   q_back.io.ren := true.B
//   q_back.io.wen := true.B

  


  
//   //fifo的enq端接cat,valid由bitmap决定
//   for(i <- 0 to 1){

//     q(i).clk_r := clk_div.io.clk_out

//     q(i).wdata     := cat.io.out
//     cat.io.ready   := !(q(i).full)  //ready的接法不对，要改，感觉不能两个接一个
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

//   q_back.io.wdata := counter.io.number_load
//   io.resp.bits.data  := q_back.io.rdata

//   //io.full_counter := q(1).full
//   io.yaofull_counter_out := q(1).yaofull

//   ss.clock := clk_div.io.clk_out
//   counter.clock := clk_div.io.clk_out

//   q(0).ren   := ss.io.ready
//   ss.io.valid         := !(q(0).empty)
//   ss.io.din           := q(0).rdata

//   q(1).ren   := counter.io.ready
//   counter.io.valid    := !(q(1).empty)
//   counter.io.din      := q(1).rdata
  
  





// }

