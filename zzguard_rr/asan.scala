package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._

class mem_ac_io extends Bundle{
    //val valid   = Bool()
    val tag     = UInt(8.W)
    val addr    = UInt(40.W)
    val cmd     = UInt(5.W)
    val size    = UInt(2.W)
}

class Asan_Imp_new(tag: Int) extends Module{
    val io = IO(new Bundle{
        //val in       = Input(UInt(32.W))
        //rocc
        // val in_addr  = Input(UInt(40.W))
        // val in_size  = Input(UInt(8.W))
        // val in_funct = Input(UInt(7.W))
        // val in_valid = Input(Bool())
        val rocc_in = Flipped(Decoupled(UInt(55.W)))
        //core0
        // val lors_valid = Input(Bool())
        // val lors_addr  = Input(UInt(40.W))
        // val ready_out  = Output(Bool())
        val din = Flipped(Decoupled(UInt(160.W)))
        val mem_acc_io = Decoupled(new mem_ac_io)

        //val funct    = Input(UInt(5.W))//5是接收初始地址，6是malloc和free访存

        // val tag      = Output(UInt(8.W))
        // val out_valid= Output(Bool())
        //val cmd      = Output(UInt(5.W))
        //val out_addr = Output(UInt(40.W))
        //val out_data = Output(UInt(8.W))

        val valid_mem   = Input(Bool())
        val data_in     = Input(UInt(8.W))
        val resp_tag    = Input(UInt(8.W))
        //val chosen      = Input(UInt(2.W))

        val can_use = Output(Bool())
        val uaf   = Output(Bool())
        val overflow= Output(Bool())


        // val out_addr = Output(UInt(32.W))
        // val out_size = Output(UInt(32.W))
        //val out_valid= Output(Bool())
    })
    dontTouch(io)
    //mask决定检测的开始和结束
    val mask = RegInit(false.B)

    //val out_valid_w = WireDefault(false.B)

    // val ready_r = RegInit(true.B)
    // val ready_rr= RegInit(true.B)val gaga = io.gaga.get
      //dontTouch(gaga)
    //val ready_w = WireDefault(true.B)
    //rocc来的数据解码
    val in_addr = io.rocc_in.bits(54,15)
    //val in_size = io.rocc_in.bits(14,7)
    val in_funct= io.rocc_in.bits(6,0)

    //data_fifo来的数据解码
    val lors_addr = io.din.bits(103,64)

    //访存的三种结果
    val can_use_r = RegInit(true.B)
    val uaf_r     = RegInit(false.B)
    val overflow_r= RegInit(false.B)
    //暂存data_fifo来的数据，用于后续比较
    //val addr_fifo_r = RegInit(0.U(40.W))
    //用一个fifo来暂存fifo来的数据，用于后续比较
    val addr_fifo = Module(new Queue(UInt(40.W),16))
    addr_fifo.io.enq.bits := lors_addr
    addr_fifo.io.enq.valid := io.din.valid

    addr_fifo.io.deq.ready := io.valid_mem && (io.resp_tag === tag.U)

    



    //在接收到一个信号之后，去访存，信号回来之前应该把ready拉低
    // when(io.din.fire && mask && (io.chosen === 1.U)){
    //     ready_r := false.B
    //     ready_rr:= false.B
    //     //addr_fifo_r := lors_addr//把访存的地址存一下，方便后面的比较
    // }
    // when(io.valid_mem && (io.resp_tag === 0.U)){
    //     ready_r := true.B
    //     ready_rr := true.B
    // }
    //io.din.ready := ready_rr
    io.din.ready := io.mem_acc_io.ready

    io.rocc_in.ready := true.B



    // val fifo_valid_out = WireDefault(false.B)
    // fifo_valid_out := q.io.deq.valid


    // val rocc_valid = WireDefault(false.B)
    // when(io.rocc_in.fire && (in_funct === 5.U)){
    //     rocc_valid := true.B
    // }
    // .otherwise{
    //     rocc_valid := false.B
    // }

    //申请shadow mem的时候，将shadow mem的起始地址作为偏移
    val offset = RegInit(0.U(40.W))
    when(io.rocc_in.fire && (in_funct === 6.U)){
        offset := in_addr
        mask   := true.B
    }
    // when(io.rocc_in.fire && (in_funct === 7.U)){
    //     //offset := in_addr
    //     mask   := true.B
    // }
    //分别算rocc来的和fifo来的地址对应的shadow mem的地址
    val fifo_addr = WireDefault(0.U(40.W))
    //val rocc_addr = WireDefault(0.U(40.W))

    fifo_addr := (lors_addr >> 5.U) + offset
    //rocc_addr := (in_addr >> 5.U) + offset

    //dontTouch(rocc_addr)
    dontTouch(fifo_addr)

    

    //out_valid_w := io.din.fire && mask

    //io.out_valid := out_valid_w
    io.mem_acc_io.valid := io.din.valid && mask
    io.mem_acc_io.bits.addr := fifo_addr
    io.mem_acc_io.bits.cmd := 0.U
    io.mem_acc_io.bits.tag := tag.U
    io.mem_acc_io.bits.size:= 0.U



    //io.out_addr := Mux(rocc_valid, rocc_addr, fifo_addr)
    //io.out_addr := fifo_addr
    //io.cmd := Mux(io.rocc_in.fire, 1.U, 0.U)
    //io.cmd := 0.U

    //store的时候，数据要延迟一个周期给,在外面延迟的
    //val data_r = RegInit(0.U(8.W))
    //data_r := in_size
    //io.out_data := in_size

    //比较的逻辑
    when(addr_fifo.io.deq.fire){
        when(io.data_in === 255.U){
            uaf_r     := true.B
            can_use_r := false.B
            overflow_r:= false.B
        }
        .elsewhen(io.data_in === 0.U){
            uaf_r     := false.B
            can_use_r := true.B
            overflow_r:= false.B
      
        }
        .elsewhen(io.data_in >= addr_fifo.io.deq.bits(4,0)){
            uaf_r     := false.B
            can_use_r := true.B
            overflow_r:= false.B
        }
        .otherwise{
            uaf_r     := false.B
            can_use_r := false.B
            overflow_r:= true.B
        }
    }
    .otherwise{
        uaf_r     := false.B
        can_use_r := true.B
        overflow_r:= false.B
    }

    io.uaf      := uaf_r
    io.can_use  := can_use_r
    io.overflow := overflow_r





    // io.tag := 0.U
    // io.cmd := 0.U
    // io.out_addr := io.in_addr
    // val data_r = RegInit(0.U(8.W))
    // data_r := io.in_size
    // io.out_data := data_r
    // when(io.in_valid){
    //     io.out_valid := true.B
    // }
    // .otherwise{
    //     io.out_valid := false.B
    // }
}

