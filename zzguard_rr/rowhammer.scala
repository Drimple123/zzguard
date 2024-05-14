package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.BufferParams.default

class rowhammer extends Module{
  val io = IO(new Bundle{
    //val valid     = Input(Bool())    // fifo valid不需要
    //val ex_reg_valid = Input(Bool()) // ex_reg_valid 不需要
    //val mem_reg_valid = Input(Bool()) // mem_reg_valid不需要
    //val wb_reg_valid   = Input(Bool()) // wb_reg_valid不需要
    //val din_opcode     = Input(UInt(7.W)) // instruction opcode 不要
    val din       =  Flipped(Decoupled(UInt(160.W))) //io.dmem.req.bits.addr 改成FLIPP
    //val dmem_resp_valid = Input(Bool()) //io.dmem.resp.valid
    //val dmem_perf_grant = Input(Bool()) //io.dmem.perf.grant
    val valid_mem = Input(Bool())
    val rowham_dmemaddr = Output(UInt(40.W)) // 填入io.dmem.req.bits.addr
    val rowham_corestalld = Output(Bool()) //停核的信号
    val rowham_req_valid = Output(Bool()) //填入io.dmem.req.valid
    //val resp_tag    = Input(UInt(8.W))
    //val ready = Output(Bool())
    val resp_tag    = Input(UInt(8.W))
   
  })
  dontTouch(io)
  io.din.ready := true.B
 val count = RegInit(0.U(10.W))
 val flashen = RegInit(false.B)
 //val ldstvalid = io.din_opcode === "b0100011".U || io.din_opcode === "b0000011".U
when(io.din.valid){
  when(count<1000.U){
    count := count + 1.U
    flashen := false.B
  }.elsewhen(count===1000.U)
  {flashen := true.B
   count := 0.U
  }.otherwise{
    flashen := false.B
  }
}.otherwise{
    flashen := false.B
}

  
  val addr = RegInit(0.U(40.W))
  when(flashen){
    addr := io.din.bits(39,0)
  }
 
 val s_idle :: s_stall :: s_cache :: s_up :: s_up2 :: s_up3 ::s_down :: Nil = Enum(7)
val rowham_valid = RegInit(false.B)
val state = RegInit(s_idle)

val rowham_dmemaddr = WireDefault(0.U(40.W))
//val rowham_data = WireDefault(0.U(64.W))
val rowham_req_valid_cache =RegInit(false.B)
val rowham_req_valid_down =RegInit(false.B)
val rowham_req_valid_up =RegInit(false.B)
val rowham_req_valid_up2 =RegInit(false.B)
val rowham_req_valid_up3 =RegInit(false.B)
val rowham_req_valid =WireDefault(false.B)
val rowham_req_valid_down_next = RegNext(rowham_req_valid_down)
io.rowham_req_valid := rowham_req_valid_cache || rowham_req_valid_down_next || rowham_req_valid_up || rowham_req_valid_up2 || rowham_req_valid_up3
//val resp_valid = Wire(Bool())
//val rowham_flashen = Wire(Bool())
//resp_valid := io.dmem.resp.valid
//rowham_flashen := io.rowham_flashen
io.rowham_corestalld := (state === s_stall || state === s_cache || state === s_up || state === s_up2 || state === s_up3|| state === s_down)
//rowham_data := io.dmem.resp.bits.data
switch(state){
    is(s_idle)
     {
      rowham_dmemaddr := 0.U
      when(flashen){
      state := s_stall
     }
    }
    is(s_stall)
     {
      rowham_dmemaddr := 0.U
      //when(!(io.ex_reg_valid || io.mem_reg_valid || io.wb_reg_valid)){
      rowham_req_valid_cache := true.B
      state := s_cache
     //}
    }
    is(s_cache)
    { rowham_req_valid_cache := false.B
      rowham_dmemaddr := addr + 2048.U 
    when(io.valid_mem){
    rowham_req_valid_up := true.B   
    state := s_up
    }
  }
    is(s_up)
    {rowham_req_valid_up := false.B
     rowham_dmemaddr := addr + 4096.U 
    when(io.valid_mem){
    rowham_req_valid_up2 := true.B
    state := s_up2
    }
  }
  is(s_up2)
    {rowham_req_valid_up2 := false.B
     rowham_dmemaddr := addr + 7144.U
    when(io.valid_mem){
    rowham_req_valid_up3 := true.B
    state := s_up3
    }
  }
  is(s_up3)
    {rowham_req_valid_up3 := false.B
     rowham_dmemaddr := addr + 8192.U
    when(io.valid_mem){
    rowham_req_valid_down := true.B
    state := s_down
    }
  }
  is(s_down)
    { rowham_req_valid_down := false.B
      rowham_dmemaddr := addr 
      when(io.valid_mem){
        state := s_idle
     
    }
   }
}

  io.rowham_dmemaddr := rowham_dmemaddr

}
