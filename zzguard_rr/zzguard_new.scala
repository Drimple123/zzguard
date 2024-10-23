package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.ClockDividerN
//lht start
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
//lht end
class zzguardrr_ram_new(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new zzguardrr_ramImp_new (this)
}

class zzguardrr_ramImp_new(outer: zzguardrr_ram_new)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  //val io = IO(new Bundle{
    //val addr        =   Input(UInt(2.W))
    val valid       =   Wire(Bool())
    val din_pc      =   Wire(UInt(40.W))
    val din_ins     =   Wire(UInt(32.W))
    val din_wdata   =   Wire(UInt(64.W))
    val din_mdata   =   Wire(UInt(64.W))
    val din_npc     =   Wire(UInt(40.W))
    val din_req_addr=   Wire(UInt(40.W))

    val cmd                     = io.cmd
    val funct                   = cmd.bits.inst.funct
    val rs2                     = cmd.bits.inst.rs2
    val rs1                     = cmd.bits.inst.rs1
    val xd                      = cmd.bits.inst.xd
    val xs1                     = cmd.bits.inst.xs1
    val xs2                     = cmd.bits.inst.xs2
    val rd                      = cmd.bits.inst.rd
    val opcode                  = cmd.bits.inst.opcode

    val rs1_val                 = cmd.bits.rs1
    val rs2_val                 = cmd.bits.rs2
    val rd_val                  = WireInit(0.U(xLen.W))
  dontTouch(cmd)  
  dontTouch(io)

  cmd.ready                  := true.B
  io.resp.bits.rd            := cmd.bits.inst.rd
  io.resp.valid              := cmd.valid
  io.busy                    := cmd.valid



  //用于asan的rocc通路
  val rocc_packet = Wire(UInt(55.W))
  val q_rocc = Module(new fifox(55, 32, 10))

  //mask,写表之前为0,写完表置1,程序运行完之后置0
  val cfg_mask = RegInit(0.U)

  //因为查表控制信号慢了1拍，所以数据也慢1拍
  val ins_r   = RegNext(din_ins,0.U)
  val wdata_r = RegNext(din_wdata,0.U)
  val mdata_r = RegNext(din_mdata,0.U)
  val valid_r = RegNext(valid,false.B)
  val npc_r = RegNext(din_npc, 0.U)
  val req_addr_r= RegNext(din_req_addr, 0.U)

  //filter的表
  val table = Module(new look_2table_ram(4))

  //打包数据的cat
  val cat2 = Module(new instruction_cat2)

  //ss的旁路的两个使能
  val j_nen = RegInit(true.B)
  val ret_en = RegInit(false.B)

  //data通路
  //q0是ss,q1是counter，q2是asan0，q3是rowhammer，q4和q5是asan1和asan2,q6和q7是counter1和2
  //q0是ss,q1到q4是asan，其他的备用
  val q = VecInit(Seq.fill(10)(Module(new fifox(64, 32, 10)).io))

  //一个宽度为4的rr_arb用于asan
  val rr_arb = Module(new zz_RRarbiter(4))

  //rocc通路，除了valid
  rocc_packet := Cat(cmd.bits.rs1(39,0), cmd.bits.rs2(7,0), cmd.bits.inst.funct)
  q_rocc.io.in.bits := rocc_packet
  io.asan_io.get <> q_rocc.io.out
  
  //给输入信号加mask
  valid       := io.valid.get & cfg_mask
  din_pc      := io.pc.get & Fill(40, cfg_mask)
  din_ins     := io.ins.get & Fill(32, cfg_mask)
  din_wdata   := io.wdata.get & Fill(64, cfg_mask)
  din_mdata   := io.mdata.get & Fill(64, cfg_mask)
  din_npc     := io.mem_npc.get & Fill(40, cfg_mask)
  din_req_addr:= io.req_addr.get & Fill(40, cfg_mask)
  
  //data通路，不包括第一个fifo，因为是ss搞个旁路
  for(i <- 1 to 9){
    q(i).in.bits := cat2.io.out
    q(i).out <> io.fifo_io.get(i)
  }

  val data_ss = Wire(UInt(64.W))
  

  q(0).in.bits := data_ss
  q(0).out <> io.fifo_io.get(0)
  

  val valid_ss = valid_r && j_nen

  //shadow stack bypass
  when(din_ins === "h8067".U){ //ret
    ret_en := true.B
    j_nen := true.B
  }
  .elsewhen((din_ins(11,0) === "h6f".U) || (din_ins(11,0) === "h67".U)){//j or jr
    j_nen := false.B
    ret_en := false.B
  }
  .otherwise{
    j_nen := true.B
    ret_en := false.B
  }

  when(ret_en){
    data_ss := cat2.io.out | "hf000_0000_0000_0000".U
  }
  .otherwise{
    data_ss := cat2.io.out
  }


  //table的io连接
  table.io.ren1 := valid
  table.io.ren2 := valid_r

  table.io.opcode   := din_ins(6,0)
  table.io.addr1    := rs1_val
  table.io.addr2    := rs1_val
  table.io.data_in1 := rs2_val
  table.io.data_in2 := rs2_val

  //cat的io连接
  cat2.io.wdata := wdata_r
  cat2.io.mdata := mdata_r
  cat2.io.npc := npc_r
  cat2.io.req_addr := req_addr_r
  cat2.io.sel := Mux(ret_en, 2.U, table.io.sel)


  //rocc指令，写表，asan, mask
  when(cmd.fire()){
    when((funct === 6.U)){//传到另一个核的asan
      q_rocc.io.in.valid := true.B
      table.io.wen1 := false.B
      table.io.wen2 := false.B
    }
    .elsewhen(funct === 4.U){//写表完成，开始检测
      q_rocc.io.in.valid := false.B
      cfg_mask := 1.U
      table.io.wen1 := false.B
      table.io.wen2 := false.B
    }
    .elsewhen(funct === 8.U){//主要程序跑完，结束检测
      q_rocc.io.in.valid := false.B
      cfg_mask := 0.U
      table.io.wen1 := false.B
      table.io.wen2 := false.B
    }
    .elsewhen(funct === 1.U){  //写第一张表
      q_rocc.io.in.valid := false.B
      table.io.wen1 := true.B
      table.io.wen2 := false.B
    }
    .elsewhen(funct === 2.U){  //写第二张表
      q_rocc.io.in.valid := false.B
      table.io.wen1 := false.B
      table.io.wen2 := true.B
    }
    .otherwise{
      q_rocc.io.in.valid := false.B
      table.io.wen1 := false.B
      table.io.wen2 := false.B
    }
  }
  .otherwise{
    q_rocc.io.in.valid := false.B
    table.io.wen1 := false.B
    table.io.wen2 := false.B
  }

  val bitmap = WireDefault(0.U(4.W))
  bitmap := table.io.bitmap

  //if ret, mark qian 4 bit of data
  

  //每组至少有一个ready就不stall
  io.fifo_ready.get := q(0).in.ready & (q(1).in.ready || q(2).in.ready || q(3).in.ready || q(4).in.ready)

  //rr_arb的io连接
  val readys = Cat(q(1).in.ready,q(2).in.ready,q(3).in.ready,q(4).in.ready)
  //valid + table + address filter
  val asan_en = valid_r && (bitmap(2) === 1.U) && ((mdata_r >= "h80004470".U) && (mdata_r <= "h80025000".U))
  rr_arb.io.req := readys & Fill(4,asan_en)
  // for((i,j) <- Seq((0,2),(1,4),(2,5),(3,8))){
  //   q(j).in.valid := rr_arb.io.gnt(i).asBool
  // }
  q(1).in.valid := rr_arb.io.gnt(0).asBool
  q(2).in.valid := rr_arb.io.gnt(1).asBool
  q(3).in.valid := rr_arb.io.gnt(2).asBool
  q(4).in.valid := rr_arb.io.gnt(3).asBool

  //ss的valid处理
  q(0).in.valid := valid_ss && (bitmap(0) === 1.U) && q(0).in.ready

  //处理一下剩下的几个valid
  for(i <- 5 to 9){
    q(i).in.valid := false.B
  }


  // val q_full_counter = RegInit(VecInit(Seq.fill(11)(0.U(32.W))))
  // dontTouch(q_full_counter)
  // for(i <- 0 to 10){
  //   when(q(i).count === 32.U){
  //     q_full_counter(i) := q_full_counter(i) + 1.U
  //   }
  // }



  
  //val rr_counter= Module(new fsm_rr_seq(Seq(1,6,7,9)))

  dontTouch(q(0))
  dontTouch(q(1))
  dontTouch(q(2))
  dontTouch(q(3))
  dontTouch(q(4))
  dontTouch(q(5))
  dontTouch(q(6))
  dontTouch(q(7))
  dontTouch(q(8))
  dontTouch(q(9))
}

