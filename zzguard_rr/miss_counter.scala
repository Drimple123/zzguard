package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class miss_counter extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool()) // 启动信号
    val b = Input(Bool()) // 结束信号
    val miss_count = Output(UInt(32.W)) // 超过5个周期的计数
    val hit_count = Output(UInt(32.W)) // 不超过5个周期的计数
  })
  dontTouch(io)
  val s_idle :: s_counting :: s_done :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val counter = RegInit(0.U(6.W))
  val c_reg = RegInit(0.U(20.W))
  val d_reg = RegInit(0.U(20.W))
  dontTouch(counter)
  io.miss_count := c_reg
  io.hit_count := d_reg

  switch(state) {
    is(s_idle) {
      when(io.a) {
        state := s_counting
        counter := 0.U
      }
    }
    is(s_counting) {
      counter := counter + 1.U
      when(io.b) {
        state := s_done
      }
    }
    is(s_done) {
      when(counter > 5.U) {
        c_reg := c_reg + 1.U
      } .otherwise {
        d_reg := d_reg + 1.U
      }
      state := s_idle
    }
  }
}

