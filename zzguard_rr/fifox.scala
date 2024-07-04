package freechips.rocketchip.zzguardrr

import chisel3._
import chisel3.util._



class filfo extends Module{
    val io = IO(new Bundle{
        val ins     = Input(UInt(32.W))
        val addr_in = Input(UInt(40.W))
        val valid_in= Input(Bool())
        val ready_stall= Output(Bool())

        val valid_out= Output(Bool())
        val data    = Output(UInt(40.W))
        val ready   = Input(Bool())
    })
    dontTouch(io)
    val filter  = Module(new asan_filter)
    val q       = Module(new Queue(UInt(40.W), 32))
    val x2one_1 = Module(new x2one(5))
    dontTouch(q.io)
    filter.io.ins := io.ins
    filter.io.addr_in := io.addr_in
    filter.io.valid_in := io.valid_in
    io.ready_stall := q.io.enq.ready

    q.io.enq.valid := filter.io.lors_valid
    q.io.enq.bits  := filter.io.addr_out
    
    q.io.deq.ready := x2one_1.io.ready_0
    x2one_1.io.valid_0 := q.io.deq.valid
    x2one_1.io.data_0 := q.io.deq.bits

    io.valid_out := x2one_1.io.valid_1
    io.data := x2one_1.io.data_1
    x2one_1.io.ready_1 := io.ready



}


class x2one(x:Int) extends Module{
    val io = IO(new Bundle{
        val ready_0 = Output(Bool())
        val valid_0 = Input(Bool())
        val data_0  = Input(UInt(40.W))

        val data_1  = Output(UInt(40.W))
        val ready_1 = Input(Bool())
        val valid_1 = Output(Bool())
    })
    dontTouch(io)
    val ready_r = RegInit(true.B)
    val valid_r = RegInit(true.B)
    val (cnt, yes) = Counter(valid_r, x)
    
    io.valid_1 := io.valid_0 && io.ready_0
    when(yes && io.ready_1){
        io.ready_0 := true.B
    }
    .otherwise{
        io.ready_0 := false.B
    }

    when(io.valid_0){
        valid_r := true.B
    }
    .elsewhen(cnt === 0.U){
        valid_r := false.B
    }
    io.data_1 := io.data_0


}

class fifox(width: Int, depth: Int, x: Int) extends Module{
    val io = IO(new Bundle{
       val in   = Flipped(Decoupled(UInt(width.W)))
       val out  = Decoupled(UInt(width.W))
       val count= Output(UInt(8.W))
    })
    dontTouch(io)
    val q = Module(new Queue(UInt(width.W), depth))
    q.io.enq <> io.in
    io.count := q.io.count
    val (cnt, yes) = Counter(true.B, x);

    io.out.bits := q.io.deq.bits
    when(cnt === 3.U){
        io.out.valid := q.io.deq.valid
        q.io.deq.ready := io.out.ready
    }
    .otherwise{
        io.out.valid := false.B
        q.io.deq.ready := false.B
    }
}

class Zzzzz_Imp extends Module{
    val io = IO(new Bundle{
        val out = Output(UInt(1.W))
    })
    dontTouch(io)
    val out_r = RegInit(0.U(1.W))
    out_r := out_r + 1.U
    io.out := out_r
}

class asan_filter extends Module{
    val io = IO(new Bundle{
        val ins     = Input(UInt(32.W))
        val addr_in = Input(UInt(40.W))
        val valid_in= Input(Bool())

        val lors_valid  = Output(Bool())
        val addr_out    = Output(UInt(40.W))
    })
    dontTouch(io)
    //io.addr_out := io.addr_in
    when(io.valid_in){
        when(io.ins(6,0) === "b0100011".U || io.ins(6,0) === "b0000011".U){
            when(io.addr_in >= "h0".U && io.addr_in < "h9000_0000".U){
                io.lors_valid := true.B
                io.addr_out   := io.addr_in
            }
            .otherwise{
                io.lors_valid := false.B
                io.addr_out := 0.U
            }
        }
        .otherwise{
            io.lors_valid := false.B
            io.addr_out := 0.U
        }
    }
    .otherwise{
        io.lors_valid := false.B
        io.addr_out := 0.U
    }
    
}

class Asan_Imp extends Module{
    val io = IO(new Bundle{
        //val in       = Input(UInt(32.W))
        //rocc
        val in_addr  = Input(UInt(40.W))
        val in_size  = Input(UInt(8.W))
        val in_funct = Input(UInt(7.W))
        val in_valid = Input(Bool())
        //core0
        val lors_valid = Input(Bool())
        val lors_addr  = Input(UInt(40.W))
        val ready_out  = Output(Bool())

        //val funct    = Input(UInt(5.W))//5是接收初始地址，6是malloc和free访存

        // val tag      = Output(UInt(8.W))
        // val out_valid= Output(Bool())
        val cmd      = Output(UInt(5.W))
        val out_addr = Output(UInt(40.W))
        val out_data = Output(UInt(8.W))

        val valid_mem   = Input(Bool())
        val data_in     = Input(UInt(8.W))

        val can_use = Output(Bool())
        val uaf   = Output(Bool())
        val overflow= Output(Bool())


        // val out_addr = Output(UInt(32.W))
        // val out_size = Output(UInt(32.W))
        val out_valid= Output(Bool())
    })
    dontTouch(io)
    //mask决定检测的开始和结束
    val mask = RegInit(false.B)

    val ready_r = RegInit(true.B)

    //访存的三种结果
    val can_use_r = RegInit(true.B)
    val uaf_r     = RegInit(false.B)
    val overflow_r= RegInit(false.B)
    
    val addr_fifo_r = RegInit(0.U(40.W))

    //接收core的load和store的fifo
    val q = Module(new Queue(UInt(40.W),32))
    q.io.enq.bits := io.lors_addr
    q.io.enq.valid := io.lors_valid && mask
    io.ready_out := q.io.enq.ready

    //在接收到一个信号之后，去访存，信号回来之前应该把ready拉低
    when(q.io.deq.valid){
        ready_r := false.B
        addr_fifo_r := q.io.deq.bits//把访存的地址存一下，方便后面的比较
    }
    when(io.valid_mem){
        ready_r := true.B
    }
    q.io.deq.ready := ready_r



    val fifo_valid_out = WireDefault(false.B)
    fifo_valid_out := q.io.deq.valid


    val rocc_valid = WireDefault(false.B)
    when(io.in_valid && (io.in_funct ===5.U)){
        rocc_valid := true.B
    }
    .otherwise{
        rocc_valid := false.B
    }

    //申请shadow mem的时候，将shadow mem的起始地址作为偏移
    val offset = RegInit(0.U(40.W))
    when(io.in_valid && (io.in_funct === 6.U)){
        offset := io.in_addr
        mask   := true.B
    }
    //分别算rocc来的和fifo来的地址对应的shadow mem的地址
    val fifo_addr = WireDefault(0.U(40.W))
    val rocc_addr = WireDefault(0.U(40.W))

    fifo_addr := (q.io.deq.bits >> 5.U) + offset
    rocc_addr := (io.in_addr >> 5.U) + offset

    dontTouch(rocc_addr)
    dontTouch(fifo_addr)

    io.out_valid := (q.io.deq.valid || rocc_valid) && mask
    io.out_addr := Mux(rocc_valid, rocc_addr, fifo_addr)
    io.cmd := Mux(rocc_valid, 1.U, 0.U)

    //store的时候，数据要延迟一个周期给
    val data_r = RegInit(0.U(8.W))
    data_r := io.in_size
    io.out_data := data_r

    //比较的逻辑
    when(io.valid_mem){
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
        .elsewhen(io.data_in >= addr_fifo_r(4,0)){
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

