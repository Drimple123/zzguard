package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._


// class Addr_extract_jal extends Module{
//   val io = IO(new Bundle{
//     val ins     = Input(UInt(32.W))
//     val addr3   = Output(SInt(40.W))
//   })
//   val addr2 = Wire(SInt(40.W))
//   val addr1 = Wire(UInt(20.W))
//   //sort the imm of the ins
//   addr1 := Cat(Cat(io.ins(31),io.ins(19,12)),Cat(io.ins(20),io.ins(30,21)))
//   //sign extend
  
//   addr2 := (Cat(Fill(20,addr1(19)),addr1) << 1).asSInt
//   io.addr3 := addr2
// }

class mini_decode extends Module{
  val io = IO(new Bundle{
    val valid     = Input(Bool())
    val din       = Input(UInt(160.W))
    val ins_type  = Output(UInt(2.W))
    val data      = Output(UInt(64.W))
  })
  //din  ins+wb+mem    wb存起来   mem比较
  //ins_type 2是ret,3是jal或jalr
  when(io.valid){
    when(io.din(159,128) ===  "h8067".U){
      io.ins_type := 2.U
      io.data     := io.din(63,0)
    }
    .elsewhen(io.din(139,135) =/= 0.U){
      io.ins_type := 3.U
      io.data     := io.din(127,64)
    }
    .otherwise{
      io.ins_type := 0.U
      io.data     := 0.U
    }
  }
  .otherwise{
    io.ins_type   := 0.U
    io.data       := 0.U
  }
  
}

class Stack(val depth: Int) extends Module {
  val io = IO(new Bundle {
    val push    = Input(Bool())
    val pop     = Input(Bool())
    val en      = Input(Bool())
    val dataIn  = Input(UInt(64.W))
    val dataOut = Output(UInt(64.W))
    val full    = Output(Bool())
    val empty   = Output(Bool())
  })
 
  val stack_mem = Mem(depth, UInt(64.W))
  val sp = RegInit(0.U(log2Ceil(depth).W))
  //val sp_2 = RegInit(0.U(log2Ceil(depth).W))
  val out = RegInit(0.U(40.W))
  val full_reg = RegInit(false.B)
  val empty_reg = RegInit(true.B)
 
  when (io.en) {
    when(io.push) {
      when(sp < depth.asUInt){
        stack_mem(sp) := io.dataIn
        sp := sp + 1.U
        //sp_2 := sp
        empty_reg := false.B
    }
      .otherwise{
        full_reg := true.B
      }
    } 
    .elsewhen(io.pop) {
      when(sp > 0.U){
      out := stack_mem(sp-1.U)
      sp := sp - 1.U
      //sp:= sp_2
      full_reg := false.B
      }
      .otherwise{
        empty_reg := true.B
      }
    }
  }
 
  io.dataOut := out
  io.full := full_reg
  io.empty := empty_reg
}

class shadow extends Module {
  val io = IO(new Bundle {
    val ins_type      = Input(UInt(2.W))
    val addr          = Input(UInt(64.W))
    val ret_correct   = Output(Bool())
  })
  dontTouch(io)
  val ins_type_reg = RegInit(0.U(2.W))
  ins_type_reg := io.ins_type
  val ins_type_reg_2 = RegInit(0.U(2.W))
  ins_type_reg_2 := ins_type_reg

  val addr_reg = RegInit(0.U(64.W))
  addr_reg := io.addr
  val addr_reg_2 = RegInit(0.U(64.W))
  addr_reg_2 := addr_reg
  //define output duiyingde reg signal
  //val is_jal_reg = RegInit(false.B)
  //val is_ret_reg = RegInit(false.B)
  val addr_ret_reg = RegInit(0.U(64.W))
  val ret_correct_reg = RegInit(true.B)
  //pipeline 2 stage
  //val is_ret_reg_2 = RegNext(is_ret_reg,false.B) 
  //val ra_reg = RegNext(io.ra,0.U)
  //val ra_reg_2 = RegNext(ra_reg,0.U)

  val stack = Module(new Stack(64))
  stack.io.en := true.B
  stack.io.dataIn := io.addr
  val pop_wire = WireDefault(false.B)
  val push_wire = WireDefault(false.B)
  stack.io.pop := pop_wire
  stack.io.push := push_wire
  addr_ret_reg := stack.io.dataOut

  //check is bu is jal or ret
  // when(((io.ins(6,0) === "b110_1111".U)||(io.ins(6,0) === "b110_0111".U))&&(io.ins =/= "h8067".U)){
  //   is_jal_reg := true.B 
  //   is_ret_reg := false.B
  //   pop_wire := false.B
  //   push_wire := true.B 
  // }
  // .elsewhen(io.ins === "h8067".U){// is ret instruction
  //   is_ret_reg := true.B 
  //   is_jal_reg := false.B
  //   pop_wire := true.B
  //   push_wire := false.B 
  // }
  // .otherwise{
  //   is_jal_reg := false.B 
  //   is_ret_reg := false.B 
  //   pop_wire := false.B
  //   push_wire := false.B 
  // }
  
  when(io.ins_type === 2.U){
    pop_wire  := true.B
    push_wire := false.B
  }
  .elsewhen(io.ins_type === 3.U){
    pop_wire  := false.B
    push_wire := true.B
  }
  .otherwise{
    pop_wire  := false.B
    push_wire := false.B
  }

  //compare ra and stack's
  when(ins_type_reg_2 === 2.U){
    when(addr_reg_2 === addr_ret_reg){
      ret_correct_reg := true.B
    }
    .otherwise{
      ret_correct_reg := false.B
    }
  }
  io.ret_correct := ret_correct_reg
}

class shadow_stack extends Module{
  val io = IO(new Bundle{
    // val valid       = Input(Bool())
    // val din         = Input(UInt(160.W))
    // val ready       = Output(Bool())
    val in = Flipped(Decoupled(UInt(160.W)))
    val ret_correct = Output(Bool())
  })
  dontTouch(io)
  io.in.ready := true.B

  val decode = Module(new mini_decode)
  decode.io.din   := io.in.bits
  decode.io.valid := io.in.valid

  val sha = Module(new shadow)
  sha.io.ins_type := decode.io.ins_type
  sha.io.addr     := decode.io.data
  io.ret_correct  := sha.io.ret_correct
}
