package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._


// class ClockDividerN(div: Int) extends BlackBox(Map("DIV" -> div)) with HasBlackBoxResource {
//   require(div > 0);
//   val io = IO(new Bundle {
//     val clk_out = Output(Clock())
//     val clk_in  = Input(Clock())
//   })
//   addResource("vsrc/ClockDividerN.sv")
// }


class gray2bin(width:Int) extends BlackBox(Map("WIDTH" -> width)) with HasBlackBoxResource{
  val io =IO(new Bundle{
    val gray = Input(UInt(width.W))
    val bin  = Output(UInt(width.W))
  })
  addResource("/vsrc/gray2bin.v")
}

class asyncfifo(depth: Int, width: Int) extends Module{
  val io = IO(new Bundle{
    val clk_r   = Input(Clock())
    val wen     = Input(Bool())
    val wdata   = Input(UInt(width.W))
    val full    = Output(Bool())
    val ren     = Input(Bool())
    val rdata   = Output(UInt(width.W))
    val empty   = Output(Bool())
    val yaofull = Output(Bool())
    val num_out = Output(UInt(6.W))
  })

  //分下clock
//   val clk_div = Module(new ClockDividerN(2))
//   val clk_r = Wire(Clock())  //读时钟
//   clk_div.io.clk_in := clock
//   clk_r := clk_div.io.clk_out
  

  dontTouch(io)
  val pw = log2Ceil(depth) + 1 //指针的位数
  val num = WireDefault(0.U(pw.W))
  val num_r = RegInit(0.U(pw.W))
  dontTouch(num)
  dontTouch(num_r)
  // val rpw = WireDefault(0.U(pw.W))
  
  // rpw(pw-1) := rpg_2(pw-1)
  // for(i<-(pw-2) to 0){
  //   rpw(i) := 
  // }
  
  val yaofull_w = WireDefault(false.B)
  io.yaofull := yaofull_w
  when(num_r >= 11.U){
    yaofull_w := true.B
  }
  .otherwise{
    yaofull_w := false.B
  }

  //val mem = VecInit(Seq.fill(depth)(Reg(UInt(width.W))))//搞一组寄存器
  val mem = RegInit(VecInit(Seq.fill(depth)(0.U(width.W))))
  dontTouch(mem)
  val wp    = RegInit(0.U(pw.W))//写指针
  val wp_n  = WireDefault(0.U(pw.W))
  val full_w  = WireDefault(false.B)
  dontTouch(full_w)
  io.full := full_w

  val wpg   = WireDefault(0.U(pw.W))

  val rp_n  = WireDefault(0.U(pw.W))
  val rpg   = WireDefault(0.U(pw.W))
  withClock(io.clk_r){
    val rp    = RegInit(0.U(pw.W))//读指针
    
    val empty_w = WireDefault(true.B)
    //val empty_r = RegInit(true.B)
    dontTouch(empty_w)
    io.empty := empty_w


    rp_n := Mux((io.ren && !empty_w), rp + 1.U, rp)//如果读了, rp+1
    rp := rp_n

    //将写指针转换为格雷码
    rpg := (rp >> 1.U) ^ rp

    //val mem_out = WireDefault(0.U)
    
    //io.rdata := mem_out
    //读fifo, 读地址要忽略rp的最高位
    io.rdata := mem(rp(pw-2, 0))
    // when(io.ren && !empty_w){
    //   mem_out := mem(rp(pw-2, 0))
    // }
    // .otherwise{
    //   io.rdata := 0.U
    // }
    //io.rdata := mem(rp(pw-2, 0))

    //wpg打两拍
    val wpg_1 = RegNext(wpg, 0.U)
    val wpg_2 = RegNext(wpg_1, 0.U)

    //判断是否空
    when(rpg === wpg_2){
        empty_w := true.B
    }
    .otherwise{
        empty_w := false.B
    }
  }

  wp_n := Mux((io.wen && !full_w), wp + 1.U, wp)//如果写了, wp+1
  wp := wp_n

  //将写指针转换为格雷码
  wpg := (wp >> 1.U) ^ wp

  //写fifo ,写地址要忽略wp的最高位
  when(io.wen && !full_w){
    mem(wp(pw-2, 0)) := io.wdata
  }

  //rpg打两拍
  val rpg_1 = RegNext(rpg, 0.U)
  val rpg_2 = RegNext(rpg_1, 0.U)

  val rp_2 = RegInit(0.U(pw.W))

  val gray2bin_1 = Module(new gray2bin(pw))
  gray2bin_1.io.gray := rpg_1
  rp_2 := gray2bin_1.io.bin
  dontTouch(rp_2)
  // //rp打两拍
  // val rp_1 = RegNext(rp_n, 0.U)
  // val rp_2 = RegNext(rp_1, 0.U)

  //计算fifo中数据量
  when(wp >= rp_2){
    num := wp - rp_2
  }
  .otherwise{
    num := wp + (1.U << pw.U) - rp_2
  }

  when(wp >= rp_2){
    num_r := wp - rp_2
  }
  .otherwise{
    num_r := wp + (1.U << pw.U) - rp_2
  }

  io.num_out := num

  //判断是否满
  when(wpg === Cat(~rpg_2(pw-1, pw-2), rpg_2(pw-3, 0))){
    full_w := true.B
  }
  .otherwise{
    full_w := false.B
  }

  //读时钟域

}

// class asyncfifo_out(depth: Int, width: Int) extends Module{
//   val io = IO(new Bundle{
//     val wen     = Input(Bool())
//     val wdata   = Input(UInt(width.W))
//     //val full    = Output(Bool())
//     val ren     = Input(Bool())
//     val rdata   = Output(UInt(width.W))
//     //val empty   = Output(Bool())
//   })
//   val fifo = Module(new asyncfifo(depth, width))
  
//   val clk_div = Module(new ClockDividerN(2))
//   val clk_r = Wire(Clock())  //读时钟
//   clk_div.io.clk_in := fifo.clock

//   fifo.io.clk_r := clk_div.io.clk_out
//   fifo.io.wen := io.wen
//   fifo.io.wdata := io.wdata
//   fifo.io.ren := io.ren
//   io.rdata := fifo.io.rdata
// }

