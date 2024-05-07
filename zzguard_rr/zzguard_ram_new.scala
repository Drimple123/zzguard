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

  val rocc_packet = Wire(UInt(55.W))
  val packet_mid  = Wire(UInt(48.W))
  packet_mid := Cat(cmd.bits.rs1(39,0), cmd.bits.rs2(7,0))
  rocc_packet := Cat(packet_mid, cmd.bits.inst.funct)
  val q_rocc = Module(new fifox(55, 32, 5))
  q_rocc.io.in.bits := rocc_packet
  io.asan_io <> q_rocc.io.out





  cmd.ready                  := true.B
  io.resp.bits.rd            := cmd.bits.inst.rd
  io.resp.valid              := cmd.valid
  //io.resp.bits.data          := counter.io.number_load
  io.busy                    := cmd.valid
  
  //mask,写表之前为0,写完表置1,程序运行完之后置0
  val cfg_mask = RegInit(0.U)

  valid       := io.valid & cfg_mask
  din_pc      := io.pc & Fill(40, cfg_mask)
  din_ins     := io.ins & Fill(32, cfg_mask)
  din_wdata   := io.wdata & Fill(64, cfg_mask)
  din_mdata   := io.mdata & Fill(64, cfg_mask)
  din_npc     := io.mem_npc & Fill(40, cfg_mask)
  din_req_addr:= io.req_addr & Fill(40, cfg_mask)
  
  //因为查表控制信号慢了1拍，所以数据也慢1拍
  val ins_r   = RegNext(din_ins,0.U)
  val wdata_r = RegNext(din_wdata,0.U)
  val mdata_r = RegNext(din_mdata,0.U)
  val valid_r = RegNext(valid,false.B)
  val npc_r = RegNext(din_npc, 0.U)
  val req_addr_r= RegNext(din_req_addr, 0.U)

  val table = Module(new look_2table_ram(4))
  table.io.ren1 := valid
  table.io.ren2 := valid_r

  table.io.opcode   := din_ins(6,0)
  table.io.addr1    := rs1_val
  table.io.addr2    := rs1_val
  table.io.data_in1 := rs2_val
  table.io.data_in2 := rs2_val

  when(cmd.fire()){
    when((funct === 5.U)||(funct === 6.U)){//传到另一个核的asan
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

  val cat = Module(new instruction_cat1)
  //cat.io.in_1  := io.din_pc
  cat.io.ins    := ins_r
  cat.io.wdata  := wdata_r
  cat.io.mdata  := mdata_r
  cat.io.npc    := npc_r
  cat.io.req_addr := req_addr_r

  cat.io.sel    := table.io.sel
  


  //val q = VecInit(Seq.fill(2)(Module(new asyncfifo(16, 160)).io))
  val q = VecInit(Seq.fill(4)(Module(new fifox(160, 32, 5)).io))
  
  //只要有一个不ready，就把主核stall住
  io.fifo_ready := q(0).in.ready && q(1).in.ready && q(2).in.ready && q(3).in.ready
  for(i<- List(0,1,3)){
    q(i).in.bits := cat.io.out
    q(i).out.ready := io.fifo_io(i).ready
    when(valid_r){
      when(bitmap(i) === 1.U){
        q(i).in.valid := true.B
      }
      .otherwise{
        q(i).in.valid := false.B
      }
    }
    .otherwise{
      q(i).in.valid := false.B
    }
    io.fifo_io(i) <> q(i).out
    dontTouch(q(i).count)
  }
  //asan要过滤一下
  q(2).in.bits := cat.io.out
  q(2).out.ready := io.fifo_io(2).ready
    when(valid_r){
      when(bitmap(2) === 1.U){
        when(mdata_r >= "h88000000".U && mdata_r <="h88100000".U){
          q(2).in.valid := true.B
        }
        .otherwise{
          q(2).in.valid := false.B
        }
      }
      .otherwise{
        q(2).in.valid := false.B
      }
    }
    .otherwise{
      q(2).in.valid := false.B
    }
    io.fifo_io(2) <> q(2).out
    dontTouch(q(2).count)

}

