package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class fsm_rr(num1: Int, num2: Int, num3: Int) extends Module {
    val io = IO(new Bundle {
        val en    = Input(Bool())
        //val valid = Output(Bool())
        val num   = Output(UInt(3.W))
    })

    //val zero :: one :: two :: Nil = Enum(3)

    // 状态寄存器
    val stateReg = RegInit(0.U(2.W))

    io.num := 0.U
    // 状态转换和输出逻辑
    switch (stateReg) {
        is(0.U) {
            when(io.en) {
                stateReg    := 1.U
                io.num      := num1.U
            }
            .otherwise{
                stateReg    := 0.U
                io.num      := num1.U
            }
        }
        is(1.U) {
            when(io.en) {
                stateReg    := 2.U
                io.num      := num2.U
            }
            .otherwise{
                stateReg    := 1.U
                io.num      := num2.U
            }
        }
        is(2.U) {
            when(io.en) {
                stateReg    := 0.U
                io.num      := num3.U
            }
            .otherwise{
                stateReg    := 2.U
                io.num      := num3.U
            }
        }
    }
}

