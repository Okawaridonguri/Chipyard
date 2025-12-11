package acustom

import chisel3._
import chisel3.util.Enum
import chisel3.util.PriorityEncoder
import chisel3.util.MuxLookup

// import org.chipsalliance.cde.config.{Config}
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._ 

// import freechips.rocketchip.util._ //log2Up 用
import chisel3.util.log2Up

import freechips.rocketchip.tilelink._

// import freechips.rocketchip.tilelink.{
//   TLNode, TLIdentityNode, TLClientNode, TLMasterParameters, TLMasterPortParameters
// }

// import freechips.rocketchip.util.GTimer




class WithaCustomAccel extends Config((site, here, up) => {

    //parameters  の追加も出来る。
    // case Sha3WidthP => 64

    case BuildRoCC => Seq((p: Parameters) => {
            val acustom = LazyModule.apply(new aCustomAccel(OpcodeSet.custom0)(p))
            acustom
        }
    )
})

class aCustomAccel(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes){
  override lazy val module = new aCustomAccelImp(this)
  // override val atlNode = TLClientNode(Seq(
  //   TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("aCustomTileLink"))),
  //   TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("aCustomTileLink2"))),

  //   ))
  //  override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
  //   name = "aCustomTileLink",
  //   supportsGet = TransferSizes.all    )))))

  // override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("aCustomTileLink")))))

  override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(
      Seq(
        TLMasterParameters.v1(
          name = "aCustomTileLink",
          sourceId = IdRange(0, 2),             // 0 と 1 を使える
        )
      )
    )
  ))
  // override val atlNode2 = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("aCustomTileLink2")))))

  // override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("CharacterCountRoCC")))))
}


class aCustomAccelImp(outer: aCustomAccel)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) 
  with HasCoreParameters 
  with HasL1CacheParameters{

  val cacheParams = tileParams.dcache.get

  val s_idle  :: s_acq :: s_gnt :: s_prep_acq :: s_prep_acq2 :: s_acq2 :: s_gnt2 :: s_tloop1 :: s_tloop2 :: s_process :: s_conti_process :: s_prep_process :: s_check :: s_check2 :: s_resp :: Nil = Enum(15)
  val state = RegInit(s_idle)
  val resp_rd = Reg(chiselTypeOf(io.resp.bits.rd))
  val ret = Reg(UInt(xLen.W))


//addr計算
  private val blockOffset = blockOffBits
  private val beatOffset = log2Up(cacheDataBits/8)


// addr1　素材
  val addr = RegInit(0.U(coreMaxAddrBits.W))
  val offset = addr(blockOffset - 1, 0)
  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)
  val next_addr = (addr_block + 1.U) << blockOffset.U

  // val recv_data = Reg(UInt(cacheDataBits.W))
  // val recv_data = RegInit(VecInit(Seq.fill(8, 8)(0.U(64.W))))//(lgCash.., 8)  ２次元配列？
  val recv_data = RegInit(0.U(512.W)) //64 byte = 512 bit
  // val recv_data = RegInit(VecInit(Seq.fill(64)(0.U)))

  val recv_beat = RegInit(0.U((cacheDataBeats+1).W))
  // val recv_beat = RegInit(0.U(log2Up(cacheDataBeats+1).W))
  // val data_bytes = VecInit(Seq.tabulate(cacheDataBits/8) { i => recv_data(8 * (i + 1) - 1, 8 * i) })
  val data_bytes = VecInit(Seq.tabulate(512/8) { i => recv_data(8 * (i + 1) - 1, 8 * i) })


  val index = RegInit(0.U(xLen.W))
  val index_save = RegInit(0.U(xLen.W))
  val recv_beat2_save = RegInit(0.U(log2Up(cacheDataBeats+1).W))

  val temp_index = RegInit(0.U(xLen.W))

//addr2 素材
  val addr2 = RegInit(0.U(coreMaxAddrBits.W))
  val offset2 = addr2(blockOffset - 1, 0)
  val addr2_block = addr2(coreMaxAddrBits - 1, blockOffset)
  val next_addr2 = (addr2_block + 1.U) << blockOffset.U


  // val recv_data2 = Reg(UInt(cacheDataBits.W))
  // val recv_data2 = RegInit(VecInit(Seq.fill(8, 8)(0.U(64.W))))//(lgCash.., 8)  ２次元配列？
  val recv_data2 = RegInit(0.U(512.W))
  // val recv_data2 = RegInit(VecInit(Seq.fill(64)(0.U)))
  val recv_beat2 = RegInit(0.U((cacheDataBeats+1).W))
  // val recv_beat2 = RegInit(0.U(log2Up(cacheDataBeats+1).W))
  // val data_bytes2 = VecInit(Seq.tabulate(cacheDataBits/8) { i => recv_data2(8 * (i + 1) - 1, 8 * i) })
  val data_bytes2 = VecInit(Seq.tabulate(512/8) { i => recv_data2(8 * (i + 1) - 1, 8 * i) })

  // val bool_table = RegInit(VecInit(Seq.fill(cacheDataBits/8)(false.B)))
  // val bool_table_next = RegInit(VecInit(Seq.fill(cacheDataBits/8)(false.B)))
  val bool_table = RegInit(VecInit(Seq.fill(cacheDataBits)(false.B)))
  val bool_table_next = RegInit(VecInit(Seq.fill(cacheDataBits)(false.B)))

  val bool_table_reduce = bool_table.reduce(_ || _)


  val zero_match  = data_bytes.map(_ === 0.U)
  val zero_match2 = data_bytes2.map(_ === 0.U)
  
  val first_zero = PriorityEncoder(zero_match) // when word is finished (\0)
  val first_zero2 = PriorityEncoder(zero_match2) // when word is finished (\0)
  val zero_found = zero_match.reduce(_ || _)
  val zero_found2 = zero_match2.reduce(_ || _)



  val needle = Reg(UInt(8.W))
  val needle_match = data_bytes2.map(_ === needle)
  val needle_found = needle_match.reduce(_ || _)

  val finished = Reg(Bool())    
  val conti_flag = Reg(Bool())    


  io.cmd.ready := (state === s_idle)
  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := ret


  when(io.cmd.fire){//state === 0

    printf(p"Fire! Start\n")
    addr := io.cmd.bits.rs1
    addr2 := io.cmd.bits.rs2
    finished := false.B
    ret := 0.U
    resp_rd := io.cmd.bits.inst.rd
    state := s_prep_acq
    printf(p"state is $state\n")
    printf(p"\n")

  }


  // when(state === s_prep_acq2){
  //   tl_out.a.bits := edgesOut.Get(
  //                 fromSource = 0.U,
  //                 toAddress = getAdd,
  //                 lgSize = lgCacheBlockBytes.U)._2
  //   state := s_acq2
  // }

  // val (tl_out, edgesOut) = outer.atlNode.out(0)
  // tl_out.a.valid := (state === s_acq) 
  // tl_out.a.bits := edgesOut.Get(
  //                     fromSource = 0.U,
  //                     toAddress = (addr_block << blockOffset),
  //                     lgSize = lgCacheBlockBytes.U)._2

  // val (tl_out2, edgesOut2) = outer.atlNode.out(1)
  // tl_out2.a.valid :=  (state === s_acq2) 
  // tl_out2.a.bits := edgesOut.Get(
  //                     fromSource = 1.U,
  //                     toAddress = (addr2_block << blockOffset),
  //                     lgSize = lgCacheBlockBytes.U)._2

  val do_acq = (state === s_acq)
  val do_acq2 = (state === s_acq2)


  val (tl_out, edgesOut) = outer.atlNode.out(0)
  // tl_out.a.valid := (state === s_acq || state === s_acq2 || state === s_prep_acq || state === s_prep_acq2)
  tl_out.a.valid := (state === s_prep_acq || state === s_prep_acq2)



  // val a_bits_0 = edgesOut.Get(
  //                       fromSource = 0.U,
  //                       toAddress = (addr_block << blockOffset) + (8.U * recv_beat),
  //                       lgSize = 1.U)._2


  // val a_bits_1 = edgesOut.Get(
  //                       fromSource = 1.U,
  //                       toAddress = (addr2_block << blockOffset) + (8.U * recv_beat2),
  //                       lgSize = 1.U)._2

  val a_bits_0 = edgesOut.Get(
                        fromSource = 0.U,
                        toAddress = (addr_block << blockOffset),
                        lgSize = 6.U)._2


  val a_bits_1 = edgesOut.Get(
                        fromSource = 1.U,
                        toAddress = (addr2_block << blockOffset),
                        lgSize = 6.U)._2
                        //対数の関係で、6.Uは64byte
                        // Portを２つにするか、2個lineを作っちゃうか

val (d_first, d_last, d_done, d_beat_count) = edgesOut.count(tl_out.d)



  // val sliced = MuxLookup(temp_index, recv_data(7,0))(Seq(
  //   0.U -> recv_data(7,0),
  //   1.U -> recv_data(15, 8),
  //   2.U -> recv_data(23,16),
  //   3.U -> recv_data(31,24),
  //   4.U -> recv_data(39,32),
  //   5.U -> recv_data(47,40),
  //   6.U -> recv_data(55,48),
  //   7.U -> recv_data(63,56)
  // ))

  //   val sliced2 = MuxLookup(temp_index, recv_data2(7,0))(Seq(
  //   0.U -> recv_data2(7,0),
  //   1.U -> recv_data2(15, 8),
  //   2.U -> recv_data2(23,16),
  //   3.U -> recv_data2(31,24),
  //   4.U -> recv_data2(39,32),
  //   5.U -> recv_data2(47,40),
  //   6.U -> recv_data2(55,48),
  //   7.U -> recv_data2(63,56)
  // ))



  when(state === s_prep_acq){
    tl_out.a.bits := a_bits_0
    temp_index := 0.U
    state := s_acq
    printf("state === s_prep_acq\n")
  }

  when(state === s_prep_acq2){
    tl_out.a.bits := a_bits_1
    temp_index := 0.U
    state := s_acq2
    printf("state === s_prep_acq2\n")

  }

  // when(state === s_prep_gnt){}


//---------------------s_acq or s_acq2-----------------------
  // when (tl_out.a.fire ||(state === s_acq || state === s_acq)) {

  when (tl_out.a.fire ) {
    printf("addr1 = %b\n", addr)
    printf("addr2 = %b\n", addr2)
    printf(p"Got tl_out.a!\n")
    state := Mux(state === s_prep_acq, s_gnt, s_gnt2)

    // printf("addr1 + recv_beat2 = %b", (addr2_block << blockOffset) + (8.U * recv_beat2))
    printf("recv_beat2 is =%b", recv_beat2)
    printf(p"state is $state\n")
    printf(p"\n")

  }
  
  tl_out.d.ready := (state === s_gnt) || (state === s_gnt2)
  val gnt = tl_out.d.bits

//---------------------s_gnt or s_gnt2-----------------------
  when (tl_out.d.fire) {//state === 2 or state === 4 これエラー
    printf(p"Got tl_out.d.fire!\n")
    printf(p"state is $state\n")

    when(state === s_gnt){
      printf(p"recv_beat + 1.U, recv_data := gnt.data\n")
      // for(i <- 0 until 7){
      //   recv_data(8 * (i + 1) - 1, 8 * i) := gnt.data
      // }
      
      recv_data := gnt.data
      printf(p"Beat: $d_beat_count, Data: ${Hexadecimal(tl_out.d.bits.data)}\n")

      when(d_last){
        state := s_prep_acq2
        printf("Transfer Done\n")
      }

      // recv_data := (recv_data << 64) + gnt.data


      // when(temp_index === 0.U){//これエラー
      //   recv_data(7,0) := gnt.data
      // }.elsewhen(temp_index === 1.U){
      //   recv_data(15, 8) := gnt.data
      // }.elsewhen(temp_index === 2.U){
      //   recv_data(23,16) := gnt.data
      // }.elsewhen(temp_index === 3.U){
      //   recv_data(31,24) := gnt.data
      // }.elsewhen(temp_index === 4.U){
      //   recv_data(39,32) := gnt.data
      // }.elsewhen(temp_index === 5.U){
      //   recv_data(47,40) := gnt.data
      // }.elsewhen(temp_index === 6.U){
      //   recv_data(55,48) := gnt.data
      // }.elsewhen(temp_index === 7.U){
      //   recv_data(63,56) := gnt.data
      // }

      // when(temp_index === 7.U){
      //   state := s_prep_acq2
      //   printf("acq!")
      // }.otherwise{
      //   state := s_tloop1
      //   printf("loop!")
      // }

      // state := Mux(temp_index === 7.U,  s_prep_acq2, s_tloop1)



    } .elsewhen(state === s_gnt2){
      printf(p"recv_beat2 + 1.U, recv_data2 := gnt.data\n")
      
      recv_data2 := gnt.data
      printf(p"Beat: $d_beat_count, Data: ${Hexadecimal(tl_out.d.bits.data)}\n")


      // when(temp_index === 0.U){
      //   recv_data2(7,0) := gnt.data
      // }.elsewhen(temp_index === 1.U){
      //   recv_data2(15, 8) := gnt.data
      // }.elsewhen(temp_index === 2.U){
      //   recv_data2(23,16) := gnt.data
      // }.elsewhen(temp_index === 3.U){
      //   recv_data2(31,24) := gnt.data
      // }.elsewhen(temp_index === 4.U){
      //   recv_data2(39,32) := gnt.data
      // }.elsewhen(temp_index === 5.U){
      //   recv_data2(47,40) := gnt.data
      // }.elsewhen(temp_index === 6.U){
      //   recv_data2(55,48) := gnt.data
      // }.elsewhen(temp_index === 7.U){
      //   recv_data2(63,56) := gnt.data
      // }



      // for(i <- 0 until 7){
      //   recv_data2(8 * (i + 1) - 1, 8 * i) := gnt.data
      // }
      // recv_data := gnt.dataasdfasdfasdf
      // sliced2 := gnt.data
      when(d_last){
        state := s_prep_process
        printf("Transfer Done\n")
      }    
    }
  
  // when(state === s_tloop1){
  //   printf("im here!")
  //   printf("recv.data1 = %b\n", recv_data)

  //   temp_index := temp_index + 1.U
  //   state := s_gnt
  // }

  // when(state === s_tloop2){
  //   printf("recv_data2 = %b\n", recv_data2)
  //   temp_index := temp_index + 1.U
  //   state := s_gnt2
  // }


    // when(state === s_gnt){
    //   printf(p"recv_beat + 1.U, recv_data := gnt.data\n")
      
    //   for(i <- 0 until 7){
    //     recv_data(8 * (i + 1) - 1, 8 * i) := gnt.data //(8 * (i + 1) - 1, 8 * i)

    //   }
    //   state := s_prep_acq2///これを、もし、acq1オンリーだったら、s_checkに行くようなフラグがいるかもしれない。

    // } .elsewhen(state === s_gnt2){
    //   printf(p"recv_beat2 + 1.U, recv_data2 := gnt.data\n")
    //   for(i <- 0 until 7){
    //     recv_data2(8 * (i + 1) - 1, 8 * i) := gnt.data
    //   }
    //   state := s_prep_process
    // }
    // printf("gnt.data = %b\n", gnt.data)
    // printf("gnt.source = %b\n", gnt.source)
    // printf("save_index === %c ", index_save)//print character 
    // printf("recv_beat2_save === %c ", recv_beat2_save)//print character 
    // printf(p"\n")
  }



  when(state === s_prep_process){
    needle := data_bytes(index)//これを全部共通にできないかなぁ
    state := s_process

    when(conti_flag && bool_table_reduce){
      index := index_save
      needle := data_bytes(index_save)
    }
  }
//---------------------s_process-----------------------


  when(state === s_process ){//state === 5
    printf(p"state is $state\n")
    printf(p"index === $index\n")
    printf("needle === %b", needle)
    printf(p"needle === $needle\n")

    when(index === 0.U){ //初期の状態("Hello"のHを見つける)
      for(i <- 0 until bool_table.length - 1){
        bool_table_next(i) := (data_bytes2(i) === needle)
      }

    }.otherwise{ //初期以降の状態（フラグが立った場所の次行だけを見る）
      for(i <- 0 until bool_table.length - 1){
          bool_table_next(i + 1) := Mux(data_bytes2(i + 1) === needle, bool_table(i), false.B)
      }
    }
    state := s_check
  }






  //---------------------s_check-----------------------

  when(state === s_check){
    bool_table := bool_table_next//これの位置は大丈夫かな？
    state := s_check2
  }


  when(state === s_check2){
    printf(p"state is $state\n")
    
    printf("bool_table =")

    for (i <- 0 until bool_table.length) {
      printf("%b", bool_table(i))
    }
    printf("\n")

    printf("bool_table_next =")

    for (i <- 0 until bool_table_next.length) {
      printf("%b", bool_table_next(i))
    }
    printf("\n")

    for(i <- 0 until bool_table.length){
      printf("%c ===", needle)//print character 
      printf("%b", data_bytes2(i))//print character 
      printf(p" (${needle === data_bytes2(i)})\n")
    }

    printf(p"bool_table_reduce =  ${bool_table_reduce}\n")//print character 
    

    when(needle === 0.U){//条件１　文字列がNULLに到達した場合
      printf("word NULL was reached. Word was found")
      // ret := 1.U
      state := s_resp

    }.elsewhen(zero_found2 && ~(bool_table_reduce)){//条件２　調査文字列にNULLが現れ、条件３を満たす場合
        printf(p"zero_found2 ON! Sentence contained NULL\n")
        state := s_resp

    }.elsewhen(~(bool_table_reduce)){//条件３フラグ配列が全て0になる
        printf(p"needle not found!\n")
        index := 0.U

        when(conti_flag){
          recv_beat2 := recv_beat2_save
          printf(p"put back recv_beat2_save")
          conti_flag := false.B

        }

        addr2 := next_addr2
        state := s_prep_acq2

        // when(recv_beat2 === cacheDataBeats.U){
        //   addr2 := next_addr2
        //   state := s_prep_acq2
        //   recv_beat2 := 0.U
        // }.otherwise{
        //   recv_beat2 := recv_beat2 + 1.U
        //   state := 
        // }

    }.elsewhen(bool_table_reduce){//条件４　フラグ配列がまだある時
      index := index + 1.U
      needle := data_bytes(index)//これを全部共通にできないかなぁ
      printf(p"needle found!\n")
      // state := s_process
      state := s_prep_process

      when(bool_table(7) === true.B){//条件５　この瞬間の状況をsaveする
        conti_flag := true.B
        printf(p"conti_flag up!")
        index_save := index
        recv_beat2_save := recv_beat2
      }
    }
    printf(p"state is $state\n")
    printf(p"finished is $finished\n")
    printf(p"\n")
  }


// Response Here
  when(io.resp.fire){
    printf(p"state is $state\n")

    printf("addr1 = %b\n", addr)
    printf("addr2 = %b\n", addr2)

    printf("resp_rd = %b\n", resp_rd)

    printf("offset = %b\n", offset)
    printf("blockOffset = %b\n", blockOffset.U)
    printf("beatOffset = %b\n", beatOffset.U)

    printf("addr_block << blockOffset = %b\n", addr_block << blockOffset)

    printf("addr_block = %b\n", addr_block)
    printf("gnt.data = %b\n", gnt.data)

    state := s_idle  
    printf("The End")
    // assert(false.B, "Simulation error")

    // printf(p"[cycle=%d]\n", GTimer())
    
  }

//これが必要か確認
  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := false.B
  tl_out.b.valid := false.B
  tl_out.c.valid := false.B
  tl_out.e.valid := false.B

}










class WithaCustomTestAccel extends Config((site, here, up) => {

    //parameters  の追加も出来る。
    // case Sha3WidthP => 64

    case BuildRoCC => Seq((p: Parameters) => {
            val acustom = LazyModule.apply(new aCustomTestAccel(OpcodeSet.custom0)(p))
            acustom
        }
    )
})

class aCustomTestAccel(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes = opcodes){
  override lazy val module = new aCustomAccelTestImp(this)
  override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("aCustomTestTileLink")))))

}

class aCustomAccelTestImp(outer: aCustomTestAccel)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) 
  with HasCoreParameters 
  with HasL1CacheParameters{


//TEST
  val cacheParams = tileParams.dcache.get

  val s_idle :: s_acq :: s_gnt :: s_check :: s_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)
  val resp_rd = Reg(chiselTypeOf(io.resp.bits.rd))
  val ret = Reg(UInt(xLen.W))

  // val addr = RegInit(0.U(coreMaxAddrBits.W))

  private val blockOffset = blockOffBits
  private val beatOffset = log2Up(cacheDataBits/8)


  val addr = RegInit(0.U(coreMaxAddrBits.W))
  val addr2 = RegInit(0.U(coreMaxAddrBits.W))

  val recv_data = Reg(UInt(cacheDataBits.W))
  val recv_beat = RegInit(0.U(log2Up(cacheDataBeats+1).W))
  val data_bytes = VecInit(Seq.tabulate(cacheDataBits/8) { i => recv_data(8 * (i + 1) - 1, 8 * i) })


  val offset = addr(blockOffset - 1, 0)
  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)

  val index = RegInit(0.U(xLen.W))
  

  io.cmd.ready := (state === s_idle)

  when(io.cmd.fire){
    addr := io.cmd.bits.rs1
    addr2 := io.cmd.bits.rs2
    resp_rd := io.cmd.bits.inst.rd
    state := s_acq
    printf(p"Fire! Start\n")
  }

  val (tl_out, edgesOut) = outer.atlNode.out(0)
  tl_out.a.valid := (state === s_acq)
  tl_out.a.bits := edgesOut.Get(
                    fromSource = 0.U,
                    toAddress = addr_block << blockOffset,
                    lgSize = lgCacheBlockBytes.U)._2
  
  when (tl_out.a.fire) {
    printf(p"Got tl_out.a!\n")
    state := s_gnt
    printf(p"state is $state\n")
    printf(p"\n")
  }

  val gnt = tl_out.d.bits
  tl_out.d.ready := (state === s_gnt)

  when(tl_out.d.fire){
      recv_beat := recv_beat + 1.U
      recv_data := gnt.data
      state := s_acq
  }

  when(state === s_check){
    when(index === 6.U){
      state := s_resp
    }.otherwise{
      state === s_gnt
      index := index + 1.U
      printf("%b", index)
    }
    printf("\n")

    for(i <- 0 until 5){
      print("%b", data_bytes(i))
    }
    printf("\n")
  }

  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd


  when(io.resp.fire){
    // printf("The end") 
    state := s_idle  
  }

  io.busy := (state =/= s_idle)//ここ違う
  io.interrupt := false.B
  io.mem.req.valid := false.B

}


/* &
Fire! Start
addr  = 0000000010000000000000101010111110000000
addr2 = 0000000010000000000000101010111101000000
resp_rd = 01010
offset = 000000
blockOffset = 110
beatOffset = 11
addr_block << blockOffset = 0000000010000000000000101010111110000000
addr_block = 0000000010000000000000101010111110
*/

    /*
Fire! Start
addr = 0000000000000000000000000000000001000001
resp_rd = 01010
offset = 000001
blockOffset = 110
beatOffset = 11
addr_block = 0000000000000000000000000000000001


    */

















/*
chisel3.util._
ー＞ Decoupled, Queue, Counter, ShiftRegister, MuxLookUp
chisel3.{Printable, VecInit}
ー＞  Printableはハードウェア上で値を取り出して表示するための型(シミュレーション専用なため、FPGAに焼いたときは消える)
printf(p"in = ${io.in}\n")
printf(p"RoCC cmd fired! rs1 = ${cmd.bits.rs1}, dataVec = $dataVec\n")

ー＞ VecInitはChiselのVecを簡単に初期化するためのUtility
val dataVec = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))

*/

/*

import freechips.rocketchip.rocket.{TLBConfig, HellaCacheArbiter}

TLBConfig RocketChipのMMU（仮想メモリ管理）関連の設定クラス
ー＞TLB(Translation Lookaside Buffer)のサイズや構造、アソシアティビティを定義する
al tlbCfg = TLBConfig(
  nSets = 32,
  nWays = 4,
  pageBytes = 4096
)

HellaCacheArbiter 
ー＞RocketChip内部のキャッシュアクセスを複数要求間で仲裁するモジュール
CPU・RoCC・PTWなど複数のマスターからキャッシュへのアクセスが同軸にく場合、誰も先に通すかを決める役割がある。
val arb = Module(new HellaCacheArbiter(nPorts = 2))
arb.io.requests(0) <> cpu.io.mem
arb.io.requests(1) <> rocc.io.mem
*/

/*
import freechips.rocketchip.rocket.constants.MemoryOpConstants
RocketChip内でメモリ操作の種類を定義した定数集
RoCCやRocketコアのロード/ストア操作、キャッシュアクセスなどで使います。
M_XRD 読み込み、M_XWR 書き込み、M_PWR　プロセッサ側のプライオリティ付き読み込み、M_PWB プロセッサが側のプライオリティ付き書き込み
val mem_cmd = Wire(UInt(3.W))
mem_cmd := M_XRD  // 読み込みコマンドをセット

*/


// import freechips.rocketchip.tilelink._


// import freechips.rocketchip.util.DecoupledHelper
// Decoupled(ready/valid) バスのハンドシェイクを簡単に書くためのヘルパー
/*
when(io.in.valid && io.in.ready) {
  // データを受け取ったときの処理
}

ー＞
val helper = DecoupledHelper(io.in.valid, io.out.ready)
when(helper.fire) {
  io.out.bits := io.in.bits
}
*/








//LazyModuleはfreechips.rocketchip.diplomacyパッケージ(パス接続フレームワーク)
// LazyModule　本体＝接続グラフを作る　　//LazyModuleImp＝実際のRTLを書く
//rocket-chip/src/main/scala/tile/LazyRoCC.scala

// ~/chipyard/generators/chipyard/src/main/scala/config/aCustomConfig.scala
// class ZstdDecompressor(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(
//     opcodes = opcodes, nPTWPorts = 12) {

// Page Table Walkerへのポート数の指定
// nPTWPorts のデフォルトは０。（RoCCからPTWにはアクセスしない）
// MMUを通したメモリアクセスをやりたいなら、数字を設定して。nPTWPorts = 1 



/*
sourve env.sh
make CONFIG=aCustomConfig
*/

/*
安全なクリーン手法
make clean　ー＞プロジェクトを初期状態に戻す
make clean-sim ー＞ C++シミュレーションモデル、Verilatorコンパイル済みファイルを削除
make clean-sim-debug ー＞　デバッグ用のビルドのみ削除

make CONFIG=aCustomConfig clean
*/


/*
/home/chukyo2/chipyard/generators/chipyard/src/main/scala/config/TutorialConfigs.scala
ここにあったやつを消した。

*/



