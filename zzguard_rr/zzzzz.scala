package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._


class Zzzzz_Imp extends Module{
    val io = IO(new Bundle{
        val out = Output(UInt(1.W))
    })
    dontTouch(io)
    val out_r = RegInit(0.U(1.W))
    out_r := out_r + 1.U
    io.out := out_r
}

