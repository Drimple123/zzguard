package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class wllf_map(val numbers: Seq[Int]) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val a = Input(Vec(numbers.length, UInt(7.W)))
    val num = Output(UInt(4.W))
  })

  // 将输入序列补齐到8个元素，补齐的部分使用 64
  val paddedA = Wire(Vec(8, UInt(7.W)))
  for (i <- 0 until 8) {
    if (i < numbers.length) {
      paddedA(i) := io.a(i)
    } else {
      paddedA(i) := 64.U
    }
  }

  // 将数字序列补齐到8个元素，并转换为4位宽的UInt
  val numVec = VecInit(numbers.padTo(8, 0).map(_.U(4.W)))

  // 初始化比较值
  val minValues = Wire(Vec(4, UInt(7.W)))
  val minIndices = Wire(Vec(4, UInt(4.W)))

  // 第一步比较，得到4个最小值和对应的索引
  for (i <- 0 until 4) {
    when(paddedA(2 * i + 1) < paddedA(2 * i)) {
      minValues(i) := paddedA(2 * i + 1)
      minIndices(i) := numVec(2 * i + 1)
    } .otherwise {
      minValues(i) := paddedA(2 * i)
      minIndices(i) := numVec(2 * i)
    }
  }

  // 第二步比较，得到2个最小值和对应的索引
  val finalMinValues = Wire(Vec(2, UInt(7.W)))
  val finalMinIndices = Wire(Vec(2, UInt(4.W)))

  for (i <- 0 until 2) {
    when(minValues(2 * i + 1) < minValues(2 * i)) {
      finalMinValues(i) := minValues(2 * i + 1)
      finalMinIndices(i) := minIndices(2 * i + 1)
    } .otherwise {
      finalMinValues(i) := minValues(2 * i)
      finalMinIndices(i) := minIndices(2 * i)
    }
  }

  // 最后一步比较，得到最小值和对应的索引
  val minValue = Wire(UInt(7.W))
  val minIndex = Wire(UInt(4.W))

  when(finalMinValues(1) < finalMinValues(0)) {
    minValue := finalMinValues(1)
    minIndex := finalMinIndices(1)
  } .otherwise {
    minValue := finalMinValues(0)
    minIndex := finalMinIndices(0)
  }

  // 输出最小值对应的num
  io.num := Mux(io.en, minIndex, 0.U)
}





