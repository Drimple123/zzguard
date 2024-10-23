// package freechips.rocketchip.zzguardrr

// import chisel3._
// import chisel3.util._
// import freechips.rocketchip.tile.ClockDividerN
// //lht start
// import freechips.rocketchip.tile._
// import freechips.rocketchip.diplomacy._
// import org.chipsalliance.cde.config.Parameters
// //lht end
// class asan_rocc(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
//   override lazy val module = new asan_rocc_Imp (this)
// }

// class asan_rocc_Imp(outer: asan_rocc)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
//     with HasCoreParameters {

//   dontTouch(io)
//     //mask决定检测的开始和结束
//     val mask = RegInit(false.B)

//     //rocc来的数据解码
//     val in_addr = io.rocc_in.get.bits(54,15)
//     val in_funct= io.rocc_in.get.bits(6,0)

//     //data_fifo来的数据解码
//     val lors_addr = io.din.get.bits(103,64)

//     //访存的三种结果
//     val can_use_r = RegInit(true.B)
//     val uaf_r     = RegInit(false.B)
//     val overflow_r= RegInit(false.B)
//     //暂存data_fifo来的数据，用于后续比较
//     val addr_fifo_r = RegInit(0.U(40.W))
//     //shadow mem的首地址，作为偏移
//     val offset = RegInit(0.U(40.W))

//     io.rocc_in.get.ready := true.B
//     //shadow mem申请好之后, 把首地址传过来作为偏移地址，把mask打开，检测开始
//     when(io.rocc_in.get.fire && (in_funct === 6.U)){
//       offset := in_addr
//       mask   := true.B
//     }
//     //把访存地址移位并加上偏移地址, 映射到shadow mem的对应位置
//     val fifo_addr = (lors_addr >> 5.U) + offset
//     dontTouch(lors_addr)
//     val s_idle :: s_read_req :: s_read_resp :: Nil = Enum(3)
//     val state = RegInit(s_idle)
//     val dmem_req = io.mem.req
//     val dmem_resp = io.mem.resp

//     val ready_r = RegInit(true.B)

//     //val addr = RegNext(fifo_addr)
//     val addr = RegInit(0.U(40.W))
//     when(io.din.get.fire && mask){
//       addr := fifo_addr
//       addr_fifo_r := lors_addr
//       ready_r := false.B
//     }
//     io.din.get.ready := ready_r

//     switch (state) {
//       is (s_idle) {
//         dmem_req.valid := false.B
//         when (io.din.get.fire && mask) {
//           state := s_read_req
//         }
//         .otherwise{
//           state := s_idle
//         }
//       }
//       is (s_read_req) {
//         dmem_req.valid := true.B
//         dmem_req.bits.addr := addr
//         //dmem_req.bits.tag := 0.U
//         dmem_req.bits.cmd := 0.U
//         dmem_req.bits.size := 0.U
//         dmem_req.bits.tag := 0.U
//         dmem_req.bits.phys := false.B //使用虚拟地址
//         dmem_req.bits.dprv := 3.U //权限为机器模式
//         dmem_req.bits.dv := false.B //读请求，数据无效
//         when (dmem_req.fire()) {
//           state := s_read_resp
//         }
//       }
//       is (s_read_resp) {
//         when (dmem_resp.valid) {
//           when(dmem_resp.bits.data === 255.U){
//           uaf_r     := true.B
//           can_use_r := false.B
//           overflow_r:= false.B
//         }
//         .elsewhen(dmem_resp.bits.data === 0.U){
//             uaf_r     := false.B
//             can_use_r := true.B
//             overflow_r:= false.B
      
//         }
//         .elsewhen(dmem_resp.bits.data >= addr_fifo_r(4,0)){
//             uaf_r     := false.B
//             can_use_r := true.B
//             overflow_r:= false.B
//         }
//         .otherwise{
//             uaf_r     := false.B
//             can_use_r := false.B
//             overflow_r:= true.B
//         }
//         dmem_req.valid := false.B
//         state := s_idle
//         ready_r := true.B
//         }
//       }
//     }
//     dontTouch(state)
//     dontTouch(can_use_r)
//     dontTouch(uaf_r)
//     dontTouch(overflow_r)

//     // when(io.valid_mem && (io.resp_tag === 0.U)){
//     //     ready_r := true.B
//     //     ready_rr := true.B
//     // }
//     // io.din.ready := ready_rr
//     // io.din.ready := io.mem_acc_io.ready

//     // io.rocc_in.ready := true.B

//     //用一个fifo来暂存fifo来的数据，用于后续比较
//     // val addr_fifo = Module(new Queue(UInt(40.W),16))
//     // addr_fifo.io.enq.bits := lors_addr
//     // addr_fifo.io.enq.valid := io.din.valid

//     // addr_fifo.io.deq.ready := io.valid_mem && (io.resp_tag === tag.U)
    
    
    



//     //在接收到一个信号之后，去访存，信号回来之前应该把ready拉低
//     // when(io.din.fire && mask && (io.chosen === 1.U)){
//     //     ready_r := false.B
//     //     ready_rr:= false.B
//     //     //addr_fifo_r := lors_addr//把访存的地址存一下，方便后面的比较
//     // }
//     // when(io.valid_mem && (io.resp_tag === 0.U)){
//     //     ready_r := true.B
//     //     ready_rr := true.B
//     // }
//     //io.din.ready := ready_rr
//     //io.din.ready := io.mem_acc_io.ready

//     //io.rocc_in.ready := true.B



//     // val fifo_valid_out = WireDefault(false.B)
//     // fifo_valid_out := q.io.deq.valid


//     // val rocc_valid = WireDefault(false.B)
//     // when(io.rocc_in.fire && (in_funct === 5.U)){
//     //     rocc_valid := true.B
//     // }
//     // .otherwise{
//     //     rocc_valid := false.B
//     // }

//     //申请shadow mem的时候，将shadow mem的起始地址作为偏移
    
//     // when(io.rocc_in.fire && (in_funct === 7.U)){
//     //     //offset := in_addr
//     //     mask   := true.B
//     // }
//     //分别算rocc来的和fifo来的地址对应的shadow mem的地址
//     //val fifo_addr = WireDefault(0.U(40.W))
//     //val rocc_addr = WireDefault(0.U(40.W))

    
//     //rocc_addr := (in_addr >> 5.U) + offset

//     //dontTouch(rocc_addr)
//     dontTouch(fifo_addr)

    

//     //out_valid_w := io.din.fire && mask

//     //io.out_valid := out_valid_w
//     // io.mem_acc_io.valid := io.din.valid && mask
//     // io.mem_acc_io.bits.addr := fifo_addr
//     // io.mem_acc_io.bits.cmd := 0.U
//     // io.mem_acc_io.bits.tag := tag.U
//     // io.mem_acc_io.bits.size:= 0.U



//     //io.out_addr := Mux(rocc_valid, rocc_addr, fifo_addr)
//     //io.out_addr := fifo_addr
//     //io.cmd := Mux(io.rocc_in.fire, 1.U, 0.U)
//     //io.cmd := 0.U

//     //store的时候，数据要延迟一个周期给,在外面延迟的
//     //val data_r = RegInit(0.U(8.W))
//     //data_r := in_size
//     //io.out_data := in_size

//     //比较的逻辑
//     // when(addr_fifo.io.deq.fire){
//     //     when(io.data_in === 255.U){
//     //         uaf_r     := true.B
//     //         can_use_r := false.B
//     //         overflow_r:= false.B
//     //     }
//     //     .elsewhen(io.data_in === 0.U){
//     //         uaf_r     := false.B
//     //         can_use_r := true.B
//     //         overflow_r:= false.B
      
//     //     }
//     //     .elsewhen(io.data_in >= addr_fifo.io.deq.bits(4,0)){
//     //         uaf_r     := false.B
//     //         can_use_r := true.B
//     //         overflow_r:= false.B
//     //     }
//     //     .otherwise{
//     //         uaf_r     := false.B
//     //         can_use_r := false.B
//     //         overflow_r:= true.B
//     //     }
//     // }
//     // .otherwise{
//     //     uaf_r     := false.B
//     //     can_use_r := true.B
//     //     overflow_r:= false.B
//     // }

//     // io.uaf      := uaf_r
//     // io.can_use  := can_use_r
//     // io.overflow := overflow_r
// }

