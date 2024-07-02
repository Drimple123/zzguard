package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class fsm_rr_seq(val numbers: Seq[Int]) extends Module {
    val io = IO(new Bundle {
        val en    = Input(Bool())
        val num   = Output(UInt(5.W))
    })

    val state = RegInit(0.U(5.W))
    val numVec = VecInit(numbers.map(_.U))
    when(io.en){
        when(state === (numbers.length -1).U){
            state := 0.U
        }
        .otherwise{
            state := state + 1.U
        }
    }
    io.num := numVec(state)
}

