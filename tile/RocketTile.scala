// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.tile

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem.HierarchicalElementCrossingParamsLike
import freechips.rocketchip.util._
import freechips.rocketchip.prci.{ClockSinkParameters}

//===== zzguardrr: Start ====//
import freechips.rocketchip.zzguardrr._
//===== zzguardrr: End   ====//

//===== zzguardrr: Start ====//
class ClockDividerN(div: Int) extends BlackBox(Map("DIV" -> div)) with HasBlackBoxResource {
  require(div > 0);
  val io = IO(new Bundle {
    val clk_out = Output(Clock())
    val clk_in  = Input(Clock())
  })
  addResource("/vsrc/ClockDividerN.sv")
}

class clkdiv2_zz extends HasBlackBoxResource{
  val io = IO(new Bundle{
    val clk_in = Input(Clock())
    val clk_out = Output(Clock())
  })
  addResource("/vsrc/clkdiv2_zz.v")
}
class clkdiv4_zz extends HasBlackBoxResource{
  val io = IO(new Bundle{
    val clk_in = Input(Clock())
    val clk_out = Output(Clock())
  })
  addResource("/vsrc/clkdiv4_zz.v")
}
//===== zzguardrr: End   ====//



case class RocketTileBoundaryBufferParams(force: Boolean = false)

case class RocketTileParams(
    core: RocketCoreParams = RocketCoreParams(),
    icache: Option[ICacheParams] = Some(ICacheParams()),
    dcache: Option[DCacheParams] = Some(DCacheParams()),
    btb: Option[BTBParams] = Some(BTBParams()),
    dataScratchpadBytes: Int = 0,
    tileId: Int = 0,
    beuAddr: Option[BigInt] = None,
    blockerCtrlAddr: Option[BigInt] = None,
    clockSinkParams: ClockSinkParameters = ClockSinkParameters(),
    boundaryBuffers: Option[RocketTileBoundaryBufferParams] = None
    ) extends InstantiableTileParams[RocketTile] {
  require(icache.isDefined)
  require(dcache.isDefined)
  val baseName = "rockettile"
  val uniqueName = s"${baseName}_$tileId"
  def instantiate(crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): RocketTile = {
    new RocketTile(this, crossing, lookup)
  }
}

class RocketTile private(
      val rocketParams: RocketTileParams,
      crossing: ClockCrossingType,
      lookup: LookupByHartIdImpl,
      q: Parameters)
    extends BaseTile(rocketParams, crossing, lookup, q)
    with SinksExternalInterrupts
    with SourcesExternalNotifications
    with HasLazyRoCC  // implies CanHaveSharedFPU with CanHavePTW with HasHellaCache
    with HasHellaCache
    with HasICacheFrontend
{
  // Private constructor ensures altered LazyModule.p is used implicitly
  def this(params: RocketTileParams, crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  val intOutwardNode = rocketParams.beuAddr map { _ => IntIdentityNode() }
  val slaveNode = TLIdentityNode()
  val masterNode = visibilityNode

  val dtim_adapter = tileParams.dcache.flatMap { d => d.scratch.map { s =>
    LazyModule(new ScratchpadSlavePort(AddressSet.misaligned(s, d.dataScratchpadBytes), lazyCoreParamsView.coreDataBytes, tileParams.core.useAtomics && !tileParams.core.useAtomicsOnlyForIO))
  }}
  dtim_adapter.foreach(lm => connectTLSlave(lm.node, lm.node.portParams.head.beatBytes))

  val bus_error_unit = rocketParams.beuAddr map { a =>
    val beu = LazyModule(new BusErrorUnit(new L1BusErrors, BusErrorUnitParams(a)))
    intOutwardNode.get := beu.intNode
    connectTLSlave(beu.node, xBytes)
    beu
  }

  //===== zzguardrr: Start ====//
  val zzzzz_3 = Module(new Zzzzz_Imp)
  //===== zzguardrr: End   ====//

  val tile_master_blocker =
    tileParams.blockerCtrlAddr
      .map(BasicBusBlockerParams(_, xBytes, masterPortBeatBytes, deadlock = true))
      .map(bp => LazyModule(new BasicBusBlocker(bp)))

  tile_master_blocker.foreach(lm => connectTLSlave(lm.controlNode, xBytes))

  // TODO: this doesn't block other masters, e.g. RoCCs
  tlOtherMastersNode := tile_master_blocker.map { _.node := tlMasterXbar.node } getOrElse { tlMasterXbar.node }
  masterNode :=* tlOtherMastersNode
  DisableMonitors { implicit p => tlSlaveXbar.node :*= slaveNode }

  nDCachePorts += 1 /*core */ + (dtim_adapter.isDefined).toInt

  val dtimProperty = dtim_adapter.map(d => Map(
    "sifive,dtim" -> d.device.asProperty)).getOrElse(Nil)

  val itimProperty = frontend.icache.itimProperty.toSeq.flatMap(p => Map("sifive,itim" -> p))

  val beuProperty = bus_error_unit.map(d => Map(
          "sifive,buserror" -> d.device.asProperty)).getOrElse(Nil)

  val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq("sifive,rocket0", "riscv")) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ cpuProperties ++ nextLevelCacheProperty
                  ++ tileProperties ++ dtimProperty ++ itimProperty ++ beuProperty)
    }
  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(tileId))
  }

  override lazy val module = new RocketTileModuleImp(this)

  override def makeMasterBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = (rocketParams.boundaryBuffers, crossing) match {
    case (Some(RocketTileBoundaryBufferParams(true )), _)                   => TLBuffer()
    case (Some(RocketTileBoundaryBufferParams(false)), _: RationalCrossing) => TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1))
    case _ => TLBuffer(BufferParams.none)
  }

  override def makeSlaveBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = (rocketParams.boundaryBuffers, crossing) match {
    case (Some(RocketTileBoundaryBufferParams(true )), _)                   => TLBuffer()
    case (Some(RocketTileBoundaryBufferParams(false)), _: RationalCrossing) => TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none)
    case _ => TLBuffer(BufferParams.none)
  }
}

class RocketTileModuleImp(outer: RocketTile) extends BaseTileModuleImp(outer)
    with HasFpuOpt
    with HasLazyRoCCModule
    with HasICacheFrontendModule {
  Annotated.params(this, outer.rocketParams)

  val core = Module(new Rocket(outer)(outer.p))

  //===== zzguardrr: Start ====//
  //给rocc模块加io的,被迫加到了coreio上，给它填上，免得作妖
  val fill_dataIO = VecInit(Seq.fill(8)(Module(new fill_laji_io(160)).io))
  for(i<-0 to 7){
    core.io.rocc.fifo_io(i) <> fill_dataIO(i).deq
  }

  val fill_asanIO = Module(new fill_laji_io(55))
  core.io.rocc.asan_io <> fill_asanIO.io.deq
  core.io.rocc.fifo_ready := false.B

  if(outer.rocketParams.tileId == 0){
    println("######zzguard###########   tileid: ",outer.rocketParams.tileId,"  ############")
    outer.ins_tile_out.get.bundle := core.io.ins
    //rocc
    // outer.addr_out.get.bundle := outer.roccs(0).module.io.asan_addr
    // outer.size_out.get.bundle := outer.roccs(0).module.io.asan_size
    // outer.valid_out.get.bundle := outer.roccs(0).module.io.asan_valid
    // outer.funct_out.get.bundle := outer.roccs(0).module.io.asan_funct
    for(i<-0 to 7){
      outer.data_bits_out_nodes.get(i).bundle:= outer.roccs(0).module.io.fifo_io(i).bits
      outer.data_valid_out_nodes.get(i).bundle:= outer.roccs(0).module.io.fifo_io(i).valid
      outer.roccs(0).module.io.fifo_io(i).ready := outer.data_ready_in_nodes.get(i).bundle

    }
    core.io.ready_stall.get := outer.roccs(0).module.io.fifo_ready
    outer.rocc_bits_out.get.bundle := outer.roccs(0).module.io.asan_io.bits
    outer.rocc_valid_out.get.bundle := outer.roccs(0).module.io.asan_io.valid
    outer.roccs(0).module.io.asan_io.ready := outer.rocc_ready_in.get.bundle

    //asan的小filter，如果ins是load or store ， valid 拉高
    // val asan_filter_1 = Module(new asan_filter)
    // asan_filter_1.io.ins := core.io.ins
    // asan_filter_1.io.addr_in := core.io.mdata
    // asan_filter_1.io.valid_in := core.io.valid

    // outer.lors_valid_out.get.bundle := asan_filter_1.io.lors_valid
    // outer.lors_addr_out.get.bundle := asan_filter_1.io.addr_out
    

    // val filfo_1 = Module(new filfo)
    // filfo_1.io.ins := core.io.ins
    // filfo_1.io.addr_in := core.io.mdata
    // filfo_1.io.valid_in := core.io.valid
    // outer.lors_valid_out.get.bundle := filfo_1.io.valid_out
    // outer.lors_addr_out.get.bundle := filfo_1.io.data
    // core.io.ready_stall.get := filfo_1.io.ready_stall
    // filfo_1.io.ready := outer.lors_ready_in.get.bundle




  
  }
  else if(outer.rocketParams.tileId == 1){
    println("######zzguard###########   tileid: ",outer.rocketParams.tileId,"  ############")
    //val zzzzzz_tile1 = Module(new Zzzzzz_Imp)
    //zzzzzz_tile1.io.in := outer.ins_tile_in.get.bundle

    // zzzzzz_tile1.io.in_addr := outer.addr_in.get.bundle
    // zzzzzz_tile1.io.in_size := outer.size_in.get.bundle
    // zzzzzz_tile1.io.in_valid := outer.valid_in.get.bundle

    //rocc
    // core.io.asan_addr.get := outer.addr_in.get.bundle
    // core.io.asan_size.get := outer.size_in.get.bundle
    // core.io.asan_valid.get := outer.valid_in.get.bundle
    // core.io.asan_funct.get := outer.funct_in.get.bundle
    // //lors
    // val q = Module(new Queue(UInt(40.W),32))
    // q.io.enq.valid := outer.lors_valid_in.get.bundle
    // q.io.enq.bits := outer.lors_addr_in.get.bundle
    // q.io.deq.ready := true.B
    // outer.lors_ready_out.get.bundle := q.io.enq.ready

    // core.io.lors_valid.get := outer.lors_valid_in.get.bundle
    // core.io.lors_addr.get := outer.lors_addr_in.get.bundle
    //outer.lors_ready_out.get.bundle := core.io.ready_out.get

    val q = VecInit(Seq.fill(8)(Module(new Queue(UInt(160.W), 32)).io))
    for(i<-0 to 7){
      dontTouch(q(i).deq)
    }
    for(i<-0 to 7){
      q(i).enq.bits := outer.data_bits_in_nodes.get(i).bundle
      q(i).enq.valid := outer.data_valid_in_nodes.get(i).bundle
      outer.data_ready_out_nodes.get(i).bundle := q(i).enq.ready
      dontTouch(q(i).count)
      //q(i).deq.ready := true.B
    }
    //q(3).deq.ready := true.B

    val q_rocc = Module(new Queue(UInt(55.W),16))
    q_rocc.io.enq.bits  := outer.rocc_bits_in.get.bundle
    q_rocc.io.enq.valid := outer.rocc_valid_in.get.bundle
    outer.rocc_ready_out.get.bundle := q_rocc.io.enq.ready
    


    dontTouch(q_rocc.io)
    //接上counter_losuan
    //val counter_losuan_1 = Module(new counter_losuan)
    val counter_ls = VecInit(Seq.fill(3)(Module(new counter_losuan).io))
    //counter_losuan_1.io.enq <> q(1).deq
    counter_ls(0).enq <> q(1).deq
    counter_ls(1).enq <> q(6).deq
    counter_ls(2).enq <> q(7).deq
    //把3个counter的结果合起来
    val num_ls = counter_ls(0).number_losuan + counter_ls(1).number_losuan + counter_ls(2).number_losuan
    dontTouch(num_ls)

    //接上ss
    val ss = Module(new shadow_stack)
    ss.io.in <> q(0).deq

    val mem_acc_fifo = VecInit(Seq.fill(3)(Module(new Queue((new mem_ac_io),16)).io))
    //接上3个asan
    val Asan_1 = Module(new Asan_Imp_new(1))
    val Asan_2 = Module(new Asan_Imp_new(3))
    val Asan_3 = Module(new Asan_Imp_new(5))

    Asan_1.io.rocc_in <> q_rocc.io.deq
    Asan_1.io.din <> q(2).deq

    Asan_2.io.rocc_in <> q_rocc.io.deq
    Asan_2.io.din <> q(4).deq

    Asan_3.io.rocc_in <> q_rocc.io.deq
    Asan_3.io.din <> q(5).deq
    
    Asan_1.io.valid_mem   := core.io.valid_mem.get
    Asan_1.io.data_in     := core.io.asan_data_out.get
    Asan_1.io.resp_tag    := core.io.resp_tag.get

    Asan_2.io.valid_mem   := core.io.valid_mem.get
    Asan_2.io.data_in     := core.io.asan_data_out.get
    Asan_2.io.resp_tag    := core.io.resp_tag.get

    Asan_3.io.valid_mem   := core.io.valid_mem.get
    Asan_3.io.data_in     := core.io.asan_data_out.get
    Asan_3.io.resp_tag    := core.io.resp_tag.get


    //Asan_1.io.chosen      := core.io.arb_chosen.get
    mem_acc_fifo(0).enq <> Asan_1.io.mem_acc_io
    core.io.mem_acc_io.get(0) <> mem_acc_fifo(0).deq

    mem_acc_fifo(1).enq <> Asan_2.io.mem_acc_io
    core.io.mem_acc_io.get(1) <> mem_acc_fifo(1).deq

    mem_acc_fifo(2).enq <> Asan_3.io.mem_acc_io
    core.io.mem_acc_io.get(2) <> mem_acc_fifo(2).deq

    //core.io.asan_valid.get := Asan_1.io.out_valid
    //core.io.asan_addr.get := Asan_1.io.out_addr
    //core.io.asan_cmd.get  := Asan_1.io.cmd
    //core.io.asan_data_in.get := Asan_1.io.out_data

    val rowhammer_1 = Module(new rowhammer)
    rowhammer_1.io.din <> q(3).deq
    val mem_acc_fifo_row = Module(new Queue((new mem_ac_io),16))
    mem_acc_fifo_row.io.enq.valid := rowhammer_1.io.rowham_req_valid
    mem_acc_fifo_row.io.enq.bits.addr := rowhammer_1.io.rowham_dmemaddr
    mem_acc_fifo_row.io.enq.bits.cmd := 0.U
    mem_acc_fifo_row.io.enq.bits.size := 0.U
    mem_acc_fifo_row.io.enq.bits.tag := 3.U
    rowhammer_1.io.valid_mem := core.io.valid_mem.get
    //rowhammer_1.io.resp_tag := core.io.resp_tag.get
    rowhammer_1.io.resp_tag := core.io.resp_tag.get
    core.io.mem_acc_io_row.get <> mem_acc_fifo_row.io.deq



    //填上tile1的不要的zzguard的ready口
    // outer.roccs(0).module.io.asan_io.ready := false.B
    // for(i<-0 to 7){
    //   outer.roccs(0).module.io.fifo_io(i).ready := false.B
    // }

    


    
  }
  //===== zzguardrr: End   ====//

  // reset vector is connected in the Frontend to s2_pc
  core.io.reset_vector := DontCare

  // Report unrecoverable error conditions; for now the only cause is cache ECC errors
  outer.reportHalt(List(outer.dcache.module.io.errors))

  // Report when the tile has ceased to retire instructions; for now the only cause is clock gating
  outer.reportCease(outer.rocketParams.core.clockGate.option(
    !outer.dcache.module.io.cpu.clock_enabled &&
    !outer.frontend.module.io.cpu.clock_enabled &&
    !ptw.io.dpath.clock_enabled &&
    core.io.cease))

  outer.reportWFI(Some(core.io.wfi))

  outer.decodeCoreInterrupts(core.io.interrupts) // Decode the interrupt vector

  outer.bus_error_unit.foreach { beu =>
    core.io.interrupts.buserror.get := beu.module.io.interrupt
    beu.module.io.errors.dcache := outer.dcache.module.io.errors
    beu.module.io.errors.icache := outer.frontend.module.io.errors
  }

  core.io.interrupts.nmi.foreach { nmi => nmi := outer.nmiSinkNode.get.bundle }

  // Pass through various external constants and reports that were bundle-bridged into the tile
  outer.traceSourceNode.bundle <> core.io.trace
  core.io.traceStall := outer.traceAuxSinkNode.bundle.stall
  outer.bpwatchSourceNode.bundle <> core.io.bpwatch
  core.io.hartid := outer.hartIdSinkNode.bundle
  require(core.io.hartid.getWidth >= outer.hartIdSinkNode.bundle.getWidth,
    s"core hartid wire (${core.io.hartid.getWidth}b) truncates external hartid wire (${outer.hartIdSinkNode.bundle.getWidth}b)")

  // Connect the core pipeline to other intra-tile modules
  outer.frontend.module.io.cpu <> core.io.imem
  dcachePorts += core.io.dmem // TODO outer.dcachePorts += () => module.core.io.dmem ??
  fpuOpt foreach { fpu =>
    core.io.fpu :<>= fpu.io.waiveAs[FPUCoreIO](_.cp_req, _.cp_resp)
    fpu.io.cp_req := DontCare
    fpu.io.cp_resp := DontCare
  }
  if (fpuOpt.isEmpty) {
    core.io.fpu := DontCare
  }
  core.io.ptw <> ptw.io.dpath

  // Connect the coprocessor interfaces
  if (outer.roccs.size > 0) {
    cmdRouter.get.io.in <> core.io.rocc.cmd
    outer.roccs.foreach{ lm =>
      lm.module.io.exception := core.io.rocc.exception
      lm.module.io.fpu_req.ready := DontCare
      lm.module.io.fpu_resp.valid := DontCare
      lm.module.io.fpu_resp.bits.data := DontCare
      lm.module.io.fpu_resp.bits.exc := DontCare
    }
    core.io.rocc.resp <> respArb.get.io.out
    core.io.rocc.busy <> (cmdRouter.get.io.busy || outer.roccs.map(_.module.io.busy).reduce(_ || _))
    core.io.rocc.interrupt := outer.roccs.map(_.module.io.interrupt).reduce(_ || _)
    (core.io.rocc.csrs zip roccCSRIOs.flatten).foreach { t => t._2 <> t._1 }

    //===== zzguardrrlht: Start ====//
    cmdRouter.get.io.valid := core.io.rocc.valid
    cmdRouter.get.io.pc := core.io.rocc.pc
    cmdRouter.get.io.ins := core.io.rocc.ins
    cmdRouter.get.io.wdata := core.io.rocc.wdata
    cmdRouter.get.io.mdata := core.io.rocc.mdata
    //core.io.yaofull_counter:= cmdRouter.get.io.yaofull_counter_out
    //core.io.rocc.yaofull_counter_out:= cmdRouter.get.io.yaofull_counter_out

    cmdRouter.get.io.mem_npc := core.io.rocc.mem_npc
    cmdRouter.get.io.req_addr := core.io.rocc.req_addr
  //===== zzguardrrlht: end ====//
  } else {
    // tie off
    core.io.rocc.cmd.ready := false.B
    core.io.rocc.resp.valid := false.B
    core.io.rocc.resp.bits := DontCare
    core.io.rocc.busy := DontCare
    core.io.rocc.interrupt := DontCare
  }

  



  // Dont care mem since not all RoCC need accessing memory
  core.io.rocc.mem := DontCare

  // Rocket has higher priority to DTIM than other TileLink clients
  outer.dtim_adapter.foreach { lm => dcachePorts += lm.module.io.dmem }

  // TODO eliminate this redundancy
  val h = dcachePorts.size
  val c = core.dcacheArbPorts
  val o = outer.nDCachePorts
  require(h == c, s"port list size was $h, core expected $c")
  require(h == o, s"port list size was $h, outer counted $o")
  // TODO figure out how to move the below into their respective mix-ins
  dcacheArb.io.requestor <> dcachePorts.toSeq
  ptw.io.requestor <> ptwPorts.toSeq

//===== zzguardrr: Start ====//
  // val zzguard = Module(new zzguardrr)
  // zzguard.io.valid      := core.io.valid
  // zzguard.io.din_pc     := core.io.pc
  // zzguard.io.din_ins    := core.io.ins
  // zzguard.io.din_wdata  := core.io.wdata
  // zzguard.io.din_mdata  := core.io.mdata
//   core.io.yaofull_counter  := zzguard.io.yaofull_counter

//   val zzguard_ram = Module(new zzguardrr_ram)
//   zzguard_ram.io.valid      := core.io.valid
//   zzguard_ram.io.din_pc     := core.io.pc
//   zzguard_ram.io.din_ins    := core.io.ins
//   zzguard_ram.io.din_wdata  := core.io.wdata
//   zzguard_ram.io.din_mdata  := core.io.mdata
//   //core.io.yaofull_counter  := zzguard_ram.io.yaofull_counter

//   when(core.io.ins(6,0) === "b0001011".U){
//     zzguard_ram.io.addr         := core.io.rs1
//     zzguard_ram.io.data_in      := core.io.rs2
//     zzguard_ram.io.funct        := core.io.ins(31,25)
//     zzguard_ram.io.valid_config := true.B
//   }
//   .otherwise{
//     zzguard_ram.io.addr         := 0.U
//     zzguard_ram.io.data_in      := 0.U
//     zzguard_ram.io.funct        := 0.U
//     zzguard_ram.io.valid_config := false.B
//   }

//===== zzguardrr: End   ====//

// }
    }
trait HasFpuOpt { this: RocketTileModuleImp =>
  val fpuOpt = outer.tileParams.core.fpu.map(params => Module(new FPU(params)(outer.p)))
}
