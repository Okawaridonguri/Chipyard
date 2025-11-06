cod//文字列を見つけるコード

class MyCustom(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRocc(opcodes){
    override lazy val module = new MyCustomModuleImp(this)
}

class MyCustomModuleImp(outer: MyCustom)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters
    with HasL1CacheParameters {
    val cacheParams = tileParams.dcache.get

    private val blockOffset = blockOffBits
    private val beatOffset = log2Up(cacheDataBits/8)

    val s_idle :: s_acq1 :: s_acq2 :: s_gnt1 :: s_gnt2 :: s_process :: s_check :: s_resp :: Nil = Enum(7)
    val state = RegInit(s_idle)
    //s_conti_process削除?

    

    val addr1_orig = Reg(UInt(coreMaxAddrBits.W))
    val addr1 = Reg(UInt(coreMaxAddrBits.W))
    val addr1_block = addr1(coreMaxAddrBits - 1, blockOffset)
    val offset1 = addr1(blockOffset - 1, 0)
    val next_addr1 = (addr1_block + 1.U) << blockOffset.U

    val recv_data1 = Reg(UInt(cacheDataBits.W))
    val recv_beat1 = RegInit(0.U(log2Up(cacheDataBeats+1).W))
    val data_bytes1 = VecInit(Seq.tabulate(cacheDataBits/8) { i => recv_data1(8 * (i + 1) - 1, 8 * i) })


    val addr2 = Reg(UInt(coreMaxAddrBits.W))
    val addr2_block = addr2(coreMaxAddrBits - 1, blockOffset)
    val offset2 = addr2(blockOffset - 1, 0)
    val next_addr2 = (addr2_block + 1.U) << blockOffset.U

    val recv_data2 = Reg(UInt(cacheDataBits.W))
    val recv_beat2 = RegInit(0.U(log2Up(cacheDataBeats+1).W))
    val data_bytes2 = VecInit(Seq.tabulate(cacheDataBits/8) { i => recv_data2(8 * (i + 1) - 1, 8 * i) })

    val resp_rd = Reg(chiselTypeOf(io.resp.bits.rd))

    val needle = Reg(UInt(8.W)) 
    val addr_count = RegInit(0.U(xLen.W))
    val bool_table = RegInit(VecInit(Seq.fill(cacheDataBits/8)(false.B)))
    val bool_table_next = RegInit(VecInit(Seq.fill(cacheDataBits/8)(false.B)))

    val index = RegInit(0.U(xLen.W))

    val getonly = RegInit(false.B)
    val conti = Reg(false.B)
    val finished = Reg(Bool())    

    val (tl_out, edgesOut) = outer.atlNode.out(0)
    val gnt = tl_out.d.bits

    val zero_match1 = data_bytes1.map(_ === 0.U)
    val zero_match2 = data_bytes2.map(_ === 0.U)


    data_bytesとbool_tableを合わせて、その次の文字だけにしておく　
    val needle_match = bool_table.map(_ === needle) 


    // val chars_found = PopCount(needle_match.zipWithIndex.map {
    //     case (matches, i) =>
    //         val idx = Cat(recv_beat1 - 1.U, i.U(beatOffset.W))
    //         matches && idx >= offset2 && i.U <= first_zero1
    // })


    val first_zero1 = PriorityEncoder(zero_match1) // when word is finished (\0)
    val first_zero2 = PriorityEncoder(zero_match2) // when word is finished (\0)
    val zero_found1 = zero_match1.reduce(_ || _)
    val zero_found2 = zero_match2.reduce(_ || _)
    val needle_found = needle_match.reduce(_ || _)

    io.cmd.ready := (state === s_idle)
    io.resp.valid := (state === s_resp)
    io.resp.bits.rd := resp_rd
    io.resp.bits.data := addr_count
    tl_out.a.valid := (state === s_acq1 || state === s_acq2)
    tl_out.a.bits := edgesOut.Get(
                       fromSource = 0.U,
                       toAddress = Mux(state === s_acq1, addr1_block, addr2_block) << blockOffset,
                       lgSize = lgCacheBlockBytes.U)._2
    tl_out.d.ready := (state === s_gnt1 || state === s_gnt2)


    when(io.cmd.fire){
        addr1 := io.cmd.bits.rs1
        addr2 := io.cmd.bits.rs2
        addr1_orig := io.cmd.bits.rs1
        resp_rd := io.cmd.bits.inst.rd
        state := s_acq1

    }

    when (tl_out.a.fire) {
        state = Mux(state === s_acq1, s_gnt1, s_gnt2)
    }

    when (tl_out.d.fire){
        when(state === s_gnt1){
            recv_beat1 := recv_beat1 + 1.U
            recv_data1 := gnt.data
            state := s_acq2

        } .elsewhen(state === s_gnt2){
            recv_beat2 := recv_beat2 + 1.U
            recv_data2 := gnt.data
            state := s_process
        }

        when(getonly){
            state := s_process
            getonly : true.B

        }
    }
//bool_tableがずらしたtrue, falseを持ち、
//そこのdata_bytesだけを見て、次のbool_tableに持ち越し、
//最後に、s_checkで確認する。
    
    when(state === s_process){
        needle = data_bytes(index)
        index := index + 1.U

        for(i <- 0 until bool_table.length - 1){
            bool_table_next(i + 1) := Mux(data_bytes2(i)(_ === needle), bool_table(i), false.B)
        }
        bool_table := bool_table_next
        state := s_check
    }


    when(state === s_check){
        when(!needle_found){
            index := 0.U
            state := s_gnt2
        }.elsewhen{
            state := s_process
        }

        when(zero_found1){// word reached \0. Check if the word is found. 

            // when(bool_table(cacheDataBeats)){ //when word was cut halfway
            //     state := s_conti_process
            // }
            when(bool_table(0, cacheDataBeats - 1).reduce( || )){//word was found. Go to response
                finished := true.B
                addr_count := index
            }
        }

        when(zero_found2){finished := true.B}// sentence reached \0. Word was not found. Go to response


        when(recv_beat1 === cacheDataBeats.U){
            addr1 := next_addr1
            state := Mux(recv_beat2 === cacheDataBeats.U, s_resp, s_acq2)
            recv_beat1 := 0.U

        } .elsewhen(recv_beat2 === cacheDataBeats.U){
            recv_beat2 := 0.U
            addr2 := next_addr2
            state := Mux(finished, s_resp, s_acq1)
        }
        

    }

    when(io.resp.fire){state := s_idle}

    io.busy := (state =/= s_idle)
    io.interrupt := false.B
    io.mem.req.valid := false.B
    // Tie off unused channels
    tl_out.b.ready := true.B
    tl_out.c.valid := false.B
    tl_out.e.valid := false.B

}




class CharacterCountExampleModuleImp(outer: CharacterCountExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
  with HasCoreParameters
  with HasL1CacheParameters {
  val cacheParams = tileParams.dcache.get

  private val blockOffset = blockOffBits
  private val beatOffset = log2Up(cacheDataBits/8)

  val needle = Reg(UInt(8.W))
  val addr = Reg(UInt(coreMaxAddrBits.W))
  val count = Reg(UInt(xLen.W))
  val resp_rd = Reg(chiselTypeOf(io.resp.bits.rd))

  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)
  val offset = addr(blockOffset - 1, 0)
  val next_addr = (addr_block + 1.U) << blockOffset.U

  val s_idle :: s_acq1 :: s_gnt :: s_check :: s_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val (tl_out, edgesOut) = outer.atlNode.out(0)
  val gnt = tl_out.d.bits
  val recv_data = Reg(UInt(cacheDataBits.W))
  val recv_beat = RegInit(0.U(log2Up(cacheDataBeats+1).W))

  val data_bytes = VecInit(Seq.tabulate(cacheDataBits/8) { i => recv_data(8 * (i + 1) - 1, 8 * i) })
  val zero_match = data_bytes.map(_ === 0.U)
  val needle_match = data_bytes.map(_ === needle)
  val first_zero = PriorityEncoder(zero_match)

  val chars_found = PopCount(needle_match.zipWithIndex.map {
    case (matches, i) =>
      val idx = Cat(recv_beat - 1.U, i.U(beatOffset.W))
      matches && idx >= offset && i.U <= first_zero
  })
  val zero_found = zero_match.reduce(_ || _)
  val finished = Reg(Bool())

  io.cmd.ready := (state === s_idle)
  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := count
  tl_out.a.valid := (state === s_acq)
  tl_out.a.bits := edgesOut.Get(
                       fromSource = 0.U,
                       toAddress = addr_block << blockOffset,
                       lgSize = lgCacheBlockBytes.U)._2
  tl_out.d.ready := (state === s_gnt)

  when (io.cmd.fire) {
    addr := io.cmd.bits.rs1
    needle := io.cmd.bits.rs2
    resp_rd := io.cmd.bits.inst.rd
    count := 0.U
    finished := false.B
    state := s_acq
  }

  when (tl_out.a.fire) { state := s_gnt }

  when (tl_out.d.fire) {
    recv_beat := recv_beat + 1.U
    recv_data := gnt.data
    state := s_check
  }

  when (state === s_check) {
    when (!finished) {
      count := count + chars_found
    }
    when (zero_found) { finished := true.B }
    when (recv_beat === cacheDataBeats.U) {
      addr := next_addr
      state := Mux(zero_found || finished, s_resp, s_acq)
      recv_beat := 0.U
    } .otherwise {
      state := s_gnt
    }
  }

  when (io.resp.fire) { state := s_idle }

  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := false.B
  // Tie off unused channels
  tl_out.b.ready := true.B
  tl_out.c.valid := false.B
  tl_out.e.valid := false.B
}































// trait HasLazyRoCCModule extends CanHavePTWModule
//     with HasCoreParameters { this: RocketTileModuleImp =>

//   val (respArb, cmdRouter) = if(outer.roccs.nonEmpty) {
//     val respArb = Module(new RRArbiter(new RoCCResponse()(outer.p), outer.roccs.size))
//     val cmdRouter = Module(new RoccCommandRouter(outer.roccs.map(_.opcodes))(outer.p))
//     outer.roccs.zipWithIndex.foreach { case (rocc, i) =>
//       rocc.module.io.ptw ++=: ptwPorts
//       rocc.module.io.cmd <> cmdRouter.io.out(i)
//       val dcIF = Module(new SimpleHellaCacheIF()(outer.p))
//       dcIF.io.requestor <> rocc.module.io.mem
//       dcachePorts += dcIF.io.cache
//       respArb.io.in(i) <> Queue(rocc.module.io.resp)
//     }
//     (Some(respArb), Some(cmdRouter))
//   } else {
//     (None, None)
//   }
//   val roccCSRIOs = outer.roccs.map(_.module.io.csrs)
// }

class AccumulatorExample(opcodes: OpcodeSet, val n: Int = 4)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new AccumulatorExampleModuleImp(this)
}

class AccumulatorExampleModuleImp(outer: AccumulatorExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  //レジスタと制御信号
  val regfile = Mem(outer.n, UInt(xLen.W)) //xLenビットのレジスタn個
  val busy = RegInit(VecInit(Seq.fill(outer.n){false.B})) //各レジスタの使用中フラグ
  //各レジスタにbusyビットがあり、ロード中はbusyになる。

  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  val addr = cmd.bits.rs2(log2Up(outer.n)-1,0)

  //命令種別　（コンピュータが実行できる命令の種類 funct）
  val doWrite = funct === 0.U //レジスタに値を書き込む　(命令が書き込み命令かを示す。)
  val doRead = funct === 1.U //レジスタから値を読みだす命令。
  val doLoad = funct === 2.U //メモリから値を読み込む、レジスタに格納命令
  val doAccum = funct === 3.U //レジスタに加算命令

/*
val a = 10 //aは10で固定される。値の定義（再代入できない定数）ワイヤや信号を定義。
funct : cmd.bits.inst.functから取り出した信号
命令に付属する関数フィールド。　RISC-V命令のfunctフィールドに相当。
=== : Chiselに使われるビット単位のTrueかfalseを出す。
functが0であればdowriteをtrue.Bに、そうでなければfalse.Bにする。
funct === 0.B はfunctと0を比較し、一致すればture.Bを返す。

0.UはChisel特有。リテラル（定数）をUInt（符号なし整数（0以上の整数飲み表現））に変換する記法
0.U : UIntの0
3.U(4.W) : 4ビット幅のUInt値3

*/

  val memRespTag = io.mem.resp.bits.tag(log2Up(outer.n)-1,0)

  // データパス処理
  val addend = cmd.bits.rs1 //レジスタに書き込む値(rs1)
  val accum = regfile(addr) 
  val wdata = Mux(doWrite, addend, accum + addend)
  // Mux(Condition, a, b) condがtrue.Bならa、false.Bならb
  //true.Bでaddend, false.Bでaccum+addend 
  //accum + addend　が加算結果

  when (cmd.fire && (doWrite || doAccum)) {
    regfile(addr) := wdata
  }

  //メモリレスポンス処理
  when (io.mem.resp.valid) {
    regfile(memRespTag) := io.mem.resp.bits.data
    busy(memRespTag) := false.B
  }
  //メモリからデータが返ってきたら、指定されたレジスタに格納し、busyを解除

  // control
  when (io.mem.req.fire) {
    busy(addr) := true.B
  }

  val doResp = cmd.bits.inst.xd
  val stallReg = busy(addr)
  val stallLoad = doLoad && !io.mem.req.ready
  val stallResp = doResp && !io.resp.ready

  cmd.ready := !stallReg && !stallLoad && !stallResp
    // command resolved if no stalls AND not issuing a load that will need a request

  //コマンド発行の可否判定
  io.resp.valid := cmd.valid && doResp && !stallReg && !stallLoad
  //レジスタがbusy出ない、メモリのreadyがある（ロード時）、応答ポートがready

  io.resp.bits.rd := cmd.bits.inst.rd
    // Must respond with the appropriate tag or undefined behavior
  io.resp.bits.data := accum
    // Semantics is to always send out prior accumulator register value

  io.busy := cmd.valid || busy.reduce(_||_)
    // Be busy when have pending memory requests or committed possibility of pending requests
  io.interrupt := false.B
    // Set this true to trigger an interrupt on the processor (please refer to supervisor documentation)

  //メモリリクエストの発行
  io.mem.req.valid := cmd.valid && doLoad && !stallReg && !stallResp
  io.mem.req.bits.addr := addend
  io.mem.req.bits.tag := addr
  io.mem.req.bits.cmd := M_XRD //　読みだし命令 perform a load (M_XWR for stores)
  // doLoad時にメモリアクセスを発行
  // addrをタグとして使い、レスポンス時のレジスタに対応付け

  io.mem.req.bits.size := log2Ceil(8).U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U // we're not performing any stores...
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := cmd.bits.status.dprv
  io.mem.req.bits.dv := cmd.bits.status.dv
  io.mem.req.bits.no_resp := false.B
}

class  TranslatorExample(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes, nPTWPorts = 1) {
  override lazy val module = new TranslatorExampleModuleImp(this)
}

class TranslatorExampleModuleImp(outer: TranslatorExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  val req_addr = Reg(UInt(coreMaxAddrBits.W))
  val req_rd = Reg(chiselTypeOf(io.resp.bits.rd))
  val req_offset = req_addr(pgIdxBits - 1, 0)
  val req_vpn = req_addr(coreMaxAddrBits - 1, pgIdxBits)
  val pte = Reg(new PTE)

  val s_idle :: s_ptw_req :: s_ptw_resp :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)

  io.cmd.ready := (state === s_idle)

  when (io.cmd.fire) {
    req_rd := io.cmd.bits.inst.rd
    req_addr := io.cmd.bits.rs1
    state := s_ptw_req
  }

  private val ptw = io.ptw(0)

  when (ptw.req.fire) { state := s_ptw_resp }

  when (state === s_ptw_resp && ptw.resp.valid) {
    pte := ptw.resp.bits.pte
    state := s_resp
  }

  when (io.resp.fire) { state := s_idle }

  ptw.req.valid := (state === s_ptw_req)
  ptw.req.bits.valid := true.B
  ptw.req.bits.bits.addr := req_vpn

  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := req_rd
  io.resp.bits.data := Mux(pte.leaf(), Cat(pte.ppn, req_offset), -1.S(xLen.W).asUInt)

  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := false.B
}

class  CharacterCountExample(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new CharacterCountExampleModuleImp(this)
  override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("CharacterCountRoCC")))))
}

class CharacterCountExampleModuleImp(outer: CharacterCountExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
  with HasCoreParameters
  with HasL1CacheParameters {
  val cacheParams = tileParams.dcache.get

  private val blockOffset = blockOffBits
  private val beatOffset = log2Up(cacheDataBits/8)

  val needle = Reg(UInt(8.W))
  val addr = Reg(UInt(coreMaxAddrBits.W))
  val count = Reg(UInt(xLen.W))
  val resp_rd = Reg(chiselTypeOf(io.resp.bits.rd))

  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)
  val offset = addr(blockOffset - 1, 0)
  val next_addr = (addr_block + 1.U) << blockOffset.U

  val s_idle :: s_acq :: s_gnt :: s_check :: s_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val (tl_out, edgesOut) = outer.atlNode.out(0)
  val gnt = tl_out.d.bits
  val recv_data = Reg(UInt(cacheDataBits.W))
  val recv_beat = RegInit(0.U(log2Up(cacheDataBeats+1).W))

  val data_bytes = VecInit(Seq.tabulate(cacheDataBits/8) { i => recv_data(8 * (i + 1) - 1, 8 * i) })
  val zero_match = data_bytes.map(_ === 0.U)
  val needle_match = data_bytes.map(_ === needle)
  val first_zero = PriorityEncoder(zero_match)

  val chars_found = PopCount(needle_match.zipWithIndex.map {
    case (matches, i) =>
      val idx = Cat(recv_beat - 1.U, i.U(beatOffset.W))
      matches && idx >= offset && i.U <= first_zero
  })
  val zero_found = zero_match.reduce(_ || _)
  val finished = Reg(Bool())

  io.cmd.ready := (state === s_idle)
  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := count
  tl_out.a.valid := (state === s_acq)
  tl_out.a.bits := edgesOut.Get(
                       fromSource = 0.U,
                       toAddress = addr_block << blockOffset,
                       lgSize = lgCacheBlockBytes.U)._2
  tl_out.d.ready := (state === s_gnt)

  when (io.cmd.fire) {
    addr := io.cmd.bits.rs1
    needle := io.cmd.bits.rs2
    resp_rd := io.cmd.bits.inst.rd
    count := 0.U
    finished := false.B
    state := s_acq
  }

  when (tl_out.a.fire) { state := s_gnt }

  when (tl_out.d.fire) {
    recv_beat := recv_beat + 1.U
    recv_data := gnt.data
    state := s_check
  }

  when (state === s_check) {
    when (!finished) {
      count := count + chars_found
    }
    when (zero_found) { finished := true.B }
    when (recv_beat === cacheDataBeats.U) {
      addr := next_addr
      state := Mux(zero_found || finished, s_resp, s_acq)
      recv_beat := 0.U
    } .otherwise {
      state := s_gnt
    }
  }

  when (io.resp.fire) { state := s_idle }

  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := false.B
  // Tie off unused channels
  tl_out.b.ready := true.B
  tl_out.c.valid := false.B
  tl_out.e.valid := false.B
}

class BlackBoxExample(opcodes: OpcodeSet, blackBoxFile: String)(implicit p: Parameters)
    extends LazyRoCC(opcodes) {
  override lazy val module = new BlackBoxExampleModuleImp(this, blackBoxFile)
}

class BlackBoxExampleModuleImp(outer: BlackBoxExample, blackBoxFile: String)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer)
    with RequireSyncReset
    with HasCoreParameters {

  val blackbox = {
    val roccIo = io
    Module(
      new BlackBox( Map( "xLen" -> IntParam(xLen),
                         "PRV_SZ" -> IntParam(PRV.SZ),
                         "coreMaxAddrBits" -> IntParam(coreMaxAddrBits),
                         "dcacheReqTagBits" -> IntParam(roccIo.mem.req.bits.tag.getWidth),
                         "M_SZ" -> IntParam(M_SZ),
                         "mem_req_bits_size_width" -> IntParam(roccIo.mem.req.bits.size.getWidth),
                         "coreDataBits" -> IntParam(coreDataBits),
                         "coreDataBytes" -> IntParam(coreDataBytes),
                         "paddrBits" -> IntParam(paddrBits),
                         "vaddrBitsExtended" -> IntParam(vaddrBitsExtended),
                         "FPConstants_RM_SZ" -> IntParam(FPConstants.RM_SZ),
                         "fLen" -> IntParam(fLen),
                         "FPConstants_FLAGS_SZ" -> IntParam(FPConstants.FLAGS_SZ)
                   ) ) with HasBlackBoxResource {
        val io = IO( new Bundle {
                      val clock = Input(Clock())
                      val reset = Input(Reset())
                      val rocc = chiselTypeOf(roccIo)
                    })
        override def desiredName: String = blackBoxFile
        addResource(s"/vsrc/$blackBoxFile.v")
      }
    )
  }

  blackbox.io.clock := clock
  blackbox.io.reset := reset
  blackbox.io.rocc.cmd <> io.cmd
  io.resp <> blackbox.io.rocc.resp
  io.mem <> blackbox.io.rocc.mem
  io.busy := blackbox.io.rocc.busy
  io.interrupt := blackbox.io.rocc.interrupt
  blackbox.io.rocc.exception := io.exception
  io.ptw <> blackbox.io.rocc.ptw
  io.fpu_req <> blackbox.io.rocc.fpu_req
  blackbox.io.rocc.fpu_resp <> io.fpu_resp

}

class OpcodeSet(val opcodes: Seq[UInt]) {
  def |(set: OpcodeSet) =
    new OpcodeSet(this.opcodes ++ set.opcodes)

  def matches(oc: UInt) = opcodes.map(_ === oc).reduce(_ || _)
}

object OpcodeSet {
  def custom0 = new OpcodeSet(Seq("b0001011".U))
  def custom1 = new OpcodeSet(Seq("b0101011".U))
  def custom2 = new OpcodeSet(Seq("b1011011".U))
  def custom3 = new OpcodeSet(Seq("b1111011".U))
  def all = custom0 | custom1 | custom2 | custom3
}

class RoccCommandRouter(opcodes: Seq[OpcodeSet])(implicit p: Parameters)
    extends CoreModule()(p) {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Vec(opcodes.size, Decoupled(new RoCCCommand))
    val busy = Output(Bool())
  })

  val cmd = Queue(io.in)
  val cmdReadys = io.out.zip(opcodes).map { case (out, opcode) =>
    val me = opcode.matches(cmd.bits.inst.opcode)
    out.valid := cmd.valid && me
    out.bits := cmd.bits
    out.ready && me
  }
  cmd.ready := cmdReadys.reduce(_ || _)
  io.busy := cmd.valid

  assert(PopCount(cmdReadys) <= 1.U,
    "Custom opcode matched for more than one accelerator")
}





//以下追加したよ


class LCM(val w : Int) extends Module{
    val io = IO(new Bundle{
        val in1 = Flipped(Valid(UInt(w.W)))
        val in2 = Flipped(Valid(UInt(w.W)))
        val out = Decoupled(UInt(w.W))
    })

    val x = Reg(UInt(w.W))
    val y = Reg(UInt(w.W))
    val a = Reg(UInt(w.W))
    val b = Reg(UInt(w.W))

    val s_idle::s_dataIn::s_gcdComp::s_lcmComp::Nil = Enum(4)
    val state = RegInit(s_idle)

    state := MuxCase(state, Seq(
        (((state===s_idle)&&io.in1.valid&&io.in2.valid) -> s_dataIn),
        ((state===s_dataIn) -> s_gcdComp),
        (((state===s_gcdComp)&&(x===y)) -> s_lcmComp),
        (((state===s_lcmComp)&&io.out.ready) -> s_idle)))

    when(state===s_dataIn){
        x := io.in1.bits
        y := io.in2.bits
        a := io.in1.bits
        b := io.in2.bits
    }

    when(state===s_gcdComp){
        when(x>=y){
            x := y
            y := x
        }.otherwise{
            y := y - x
        }
    }

    io.out.bits := a * b / x
    io.out.valid := state===s_lcmComp
}

class LCMRoCCAccel(opcodes: OpcodeSet, val w: Int)(implicit p: Parameters) extends LazyRoCC(opcodes){
    override lazy val module = new LazyRoCCModuleImp(this){

        val rd = RegInit(0.U(5.W))
        val rs1Value = RegInit(0.U(w.W))
        val rs1Enable = RegInit(false.B)
        val rs2Value = RegInit(0.U(w.W))
        val rs2Enable = RegInit(false.B)

        val busy = RegInit(false.B)
        val canResp = RegInit(false.B)
        io.cmd.ready := !busy
        io.busy := busy
        
        val canDecode = io.cmd.fire && (io.cmd.bits.inst.funct===0.U)
        when(canDecode){
            busy := true.B
            rs1Value := io.cmd.bits.rs1
            rs1Enable := true.B
            rs2Value := io.cmd.bits.rs2
            rs2Enable := true.B
            rd := io.cmd.bits.inst.rd
        }
        val lcm = Module(new LCM(w))
        lcm.io.in1.bits := rs1Value
        lcm.io.in2.bits := rs2Value
        lcm.io.in1.valid := rs1Enable
        lcm.io.in2.valid := rs2Enable  

        val lcmRes = RegInit(0.U(w.W))

        lcm.io.out.ready := Mux(lcm.io.out.valid, true.B, false.B)
        when(lcm.io.out.valid){
            lcmRes := lcm.io.out.bits
            canResp := true.B
        }

        io.resp.valid := canResp
        io.resp.bits.rd := rd
        io.resp.bits.data := lcmRes
        when(io.resp.fire){
            canResp := false.B
            busy := false.B
            rs1Enable := false.B
            rs2Enable := false.B
            rs1Value := 0.U
            rs2Value := 0.U
            lcmRes := 0.U

        }

    }

}







#include <stdio.h>
#include "rocc.h"

/*
rs1、rs2を決める。
~/chipyard/sims/firesim/sim/rocket-chip/src/main/scala/subsystem/Configs.scala
WithRoccExampleのクラスプログラムに、4つの命令を決める場所があるかも。調査

*/

static inline unsigned long find_word(int rs1, int rs2){

    unsigned long found; 
    asm volatile("fence");
    // OCC_INSTRUCTION_DSS(X, rd, rs1, rs2, funct)
    //[ funct7 ][ rs2 ][ rs1 ][ funct3 ][ rd ][ opcode ]
    //7bit 5bit 5bit 3bit 5bit 7bit

    ROCC_INSTRUCTION_DSS(0, found, rs1, rs2 ,0);
    return found;
}
char str[64] __attribute__ ((aligned(64))) = "ABCDEFGHelloEFG";
char word[] = "Hello";

// DSS命令の場合、rs1は5bit
//メモリは32ビット

cpuのレジスタ番号をｒｓに
れじすたから持ってくる奴を意識。
ロケットコアで拡張命令を見つける。の卒業論文見つけてみる。調べてみる。　英語で。
rocket core を調べてみる。





int main(void){
    int rs1 = 0;
    int rs2 = 0;

    for(int i = 0; i < sizeof(str); i++){
        if(find_word(rs1, rs2)){
            printf("%s was found", word);
            return 1; 
        } 
    }
    printf("%s was not found", word);
    return 0; 
}


// FPGA & Vivado
// レジスタレベルのプログラミング
//




// Based on code by Schuyler Eldridge. Copyright (c) Boston University
// https://github.com/seldridge/rocket-rocc-examples/blob/master/src/main/c/rocc.h

#ifndef SRC_MAIN_C_ROCC_H
#define SRC_MAIN_C_ROCC_H

#include <stdint.h>

#define STR1(x) #x
#define STR(x) STR1(x)
#define EXTRACT(a, size, offset) (((~(~0 << size) << offset) & a) >> offset)

#define CUSTOMX_OPCODE(x) CUSTOM_ ## x
#define CUSTOM_0 0b0001011
#define CUSTOM_1 0b0101011
#define CUSTOM_2 0b1011011
#define CUSTOM_3 0b1111011

#define CUSTOMX(X, xd, xs1, xs2, rd, rs1, rs2, funct) \
  CUSTOMX_OPCODE(X)                     |             \
  (rd                 << (7))           |             \
  (xs2                << (7+5))         |             \
  (xs1                << (7+5+1))       |             \
  (xd                 << (7+5+2))       |             \
  (rs1                << (7+5+3))       |             \
  (rs2                << (7+5+3+5))     |             \
  (EXTRACT(funct, 7, 0) << (7+5+3+5+5))

// Standard macro that passes rd, rs1, and rs2 via registers
#define ROCC_INSTRUCTION_DSS(X, rd, rs1, rs2, funct) \
	ROCC_INSTRUCTION_R_R_R(X, rd, rs1, rs2, funct, 10, 11, 12)

#define ROCC_INSTRUCTION_DS(X, rd, rs1, funct) \
	ROCC_INSTRUCTION_R_R_I(X, rd, rs1, 0, funct, 10, 11)

#define ROCC_INSTRUCTION_D(X, rd, funct) \
	ROCC_INSTRUCTION_R_I_I(X, rd, 0, 0, funct, 10)

#define ROCC_INSTRUCTION_SS(X, rs1, rs2, funct) \
	ROCC_INSTRUCTION_I_R_R(X, 0, rs1, rs2, funct, 11, 12)

#define ROCC_INSTRUCTION_S(X, rs1, funct) \
	ROCC_INSTRUCTION_I_R_I(X, 0, rs1, 0, funct, 11)

#define ROCC_INSTRUCTION(X, funct) \
	ROCC_INSTRUCTION_I_I_I(X, 0, 0, 0, funct)

// rd, rs1, and rs2 are data
// rd_n, rs_1, and rs2_n are the register numbers to use
#define ROCC_INSTRUCTION_R_R_R(X, rd, rs1, rs2, funct, rd_n, rs1_n, rs2_n) { \
    register uint64_t rd_  asm ("x" # rd_n);                                 \
    register uint64_t rs1_ asm ("x" # rs1_n) = (uint64_t) rs1;               \
    register uint64_t rs2_ asm ("x" # rs2_n) = (uint64_t) rs2;               \
    asm volatile (                                                           \
        ".word " STR(CUSTOMX(X, 1, 1, 1, rd_n, rs1_n, rs2_n, funct)) "\n\t"  \
        : "=r" (rd_)                                                         \
        : [_rs1] "r" (rs1_), [_rs2] "r" (rs2_));                             \
    rd = rd_;                                                                \
  }

#define ROCC_INSTRUCTION_R_R_I(X, rd, rs1, rs2, funct, rd_n, rs1_n) {     \
    register uint64_t rd_  asm ("x" # rd_n);                              \
    register uint64_t rs1_ asm ("x" # rs1_n) = (uint64_t) rs1;            \
    asm volatile (                                                        \
        ".word " STR(CUSTOMX(X, 1, 1, 0, rd_n, rs1_n, rs2, funct)) "\n\t" \
        : "=r" (rd_) : [_rs1] "r" (rs1_));                                \
    rd = rd_;                                                             \
  }

#define ROCC_INSTRUCTION_R_I_I(X, rd, rs1, rs2, funct, rd_n) {           \
    register uint64_t rd_  asm ("x" # rd_n);                             \
    asm volatile (                                                       \
        ".word " STR(CUSTOMX(X, 1, 0, 0, rd_n, rs1, rs2, funct)) "\n\t"  \
        : "=r" (rd_));                                                   \
    rd = rd_;                                                            \
  }

#define ROCC_INSTRUCTION_I_R_R(X, rd, rs1, rs2, funct, rs1_n, rs2_n) {    \
    register uint64_t rs1_ asm ("x" # rs1_n) = (uint64_t) rs1;            \
    register uint64_t rs2_ asm ("x" # rs2_n) = (uint64_t) rs2;            \
    asm volatile (                                                        \
        ".word " STR(CUSTOMX(X, 0, 1, 1, rd, rs1_n, rs2_n, funct)) "\n\t" \
        :: [_rs1] "r" (rs1_), [_rs2] "r" (rs2_));                         \
  }

#define ROCC_INSTRUCTION_I_R_I(X, rd, rs1, rs2, funct, rs1_n) {         \
    register uint64_t rs1_ asm ("x" # rs1_n) = (uint64_t) rs1;          \
    asm volatile (                                                      \
        ".word " STR(CUSTOMX(X, 0, 1, 0, rd, rs1_n, rs2, funct)) "\n\t" \
        :: [_rs1] "r" (rs1_));                                          \
  }

#define ROCC_INSTRUCTION_I_I_I(X, rd, rs1, rs2, funct) {                 \
    asm volatile (                                                       \
        ".word " STR(CUSTOMX(X, 0, 0, 0, rd, rs1, rs2, funct)) "\n\t" ); \
  }

#endif  // SRC_MAIN_C_ACCUMULATOR_H


#include "rocc.h"

char string[64] __attribute__ ((aligned (64))) = "The quick brown fox jumped over the lazy dog";

static inline unsigned long count_chars(char *start, char needle)
{
	unsigned long count;
	asm volatile ("fence");
	ROCC_INSTRUCTION_DSS(2, count, start, needle, 0);
	return count;
}

int main(void)
{
	unsigned long count = count_chars(string + 14, 'o');
	if (count != 3)
		return count + 1;
	return 0;
}
