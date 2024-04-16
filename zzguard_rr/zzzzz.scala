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
class Zzzzzz_Imp extends Module{
    val io = IO(new Bundle{
        val in       = Input(UInt(32.W))
        val in_addr  = Input(UInt(40.W))
        val in_size  = Input(UInt(8.W))
        val in_valid = Input(Bool())
        // val out_addr = Output(UInt(32.W))
        // val out_size = Output(UInt(32.W))
        // val out_vlaid= Output(Bool())
    })
    dontTouch(io)
    
}

