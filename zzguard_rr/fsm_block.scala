package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class fsm_block extends Module {
    val io = IO(new Bundle {
        val en    = Input(Bool())
        //val valid = Output(Bool())
        val num   = Output(UInt(3.W))
    })

    //val zero :: one :: two :: Nil = Enum(3)

    // 状态寄存器
    val stateReg = RegInit(0.U(4.W))

    io.num := 0.U
    // 状态转换和输出逻辑
    switch (stateReg) {
        is(0.U) {
            when(io.en) {
                stateReg    := 1.U
                io.num      := 3.U
            }
            .otherwise{
                stateReg    := 0.U
                io.num      := 3.U
            }
        }
        is(1.U) {
            when(io.en) {
                stateReg    := 2.U
                io.num      := 3.U
            }
            .otherwise{
                stateReg    := 1.U
                io.num      := 3.U
            }
        }
        is(2.U) {
            when(io.en) {
                stateReg    := 3.U
                io.num      := 3.U
            }
            .otherwise{
                stateReg    := 2.U
                io.num      := 3.U
            }
        }
        is(3.U) {
            when(io.en) {
                stateReg    := 4.U
                io.num      := 4.U
            }
            .otherwise{
                stateReg    := 3.U
                io.num      := 4.U
            }
        }
        is(4.U) {
            when(io.en) {
                stateReg    := 5.U
                io.num      := 4.U
            }
            .otherwise{
                stateReg    := 4.U
                io.num      := 4.U
            }
        }
        is(5.U) {
            when(io.en) {
                stateReg    := 6.U
                io.num      := 4.U
            }
            .otherwise{
                stateReg    := 5.U
                io.num      := 4.U
            }
        }
        is(6.U) {
            when(io.en) {
                stateReg    := 7.U
                io.num      := 5.U
            }
            .otherwise{
                stateReg    := 6.U
                io.num      := 5.U
            }
        }
        is(7.U) {
            when(io.en) {
                stateReg    := 8.U
                io.num      := 5.U
            }
            .otherwise{
                stateReg    := 7.U
                io.num      := 5.U
            }
        }
        is(8.U) {
            when(io.en) {
                stateReg    := 0.U
                io.num      := 5.U
            }
            .otherwise{
                stateReg    := 8.U
                io.num      := 5.U
            }
        }

    }
}

