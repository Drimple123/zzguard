package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.ClockDividerN
//lht start
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
//lht end
class asan_rocc(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new asan_rocc_Imp (this)
}

class asan_rocc_Imp(outer: asan_rocc)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

      val gaga = io.gaga.get
      dontTouch(gaga)
}

