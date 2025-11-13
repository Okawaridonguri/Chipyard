package acustom

import chisel3._
import chisel3.util.Enum
import chisel3.util.PriorityEncoder

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
  override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("aCustomTileLink")))))

  // override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("CharacterCountRoCC")))))
}


class aCustomAccelImp(outer: aCustomAccel)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) 
  with HasCoreParameters 
  with HasL1CacheParameters{

  val cacheParams = tileParams.dcache.get

  val s_idle  :: s_acq :: s_gnt :: s_acq2 :: s_gnt2 :: s_process :: s_check :: s_resp :: Nil = Enum(8)
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

  val recv_data = Reg(UInt(cacheDataBits.W))
  val recv_beat = RegInit(0.U(log2Up(cacheDataBeats+1).W))
  val data_bytes = VecInit(Seq.tabulate(cacheDataBits/8) { i => recv_data(8 * (i + 1) - 1, 8 * i) })


//addr2 素材
  val addr2 = RegInit(0.U(coreMaxAddrBits.W))
  val offset2 = addr2(blockOffset - 1, 0)
  val addr2_block = addr2(coreMaxAddrBits - 1, blockOffset)
  val next_addr2 = (addr2_block + 1.U) << blockOffset.U


  val recv_data2 = Reg(UInt(cacheDataBits.W))
  val recv_beat2 = RegInit(0.U(log2Up(cacheDataBeats+1).W))
  val data_bytes2 = VecInit(Seq.tabulate(cacheDataBits/8) { i => recv_data2(8 * (i + 1) - 1, 8 * i) })

  val bool_table = RegInit(VecInit(Seq.fill(cacheDataBits/8)(false.B)))
  val bool_table_next = RegInit(VecInit(Seq.fill(cacheDataBits/8)(false.B)))



  val zero_match  = data_bytes.map(_ === 0.U)
  val zero_match2 = data_bytes2.map(_ === 0.U)
  
  val first_zero = PriorityEncoder(zero_match) // when word is finished (\0)
  val first_zero2 = PriorityEncoder(zero_match2) // when word is finished (\0)
  val zero_found = zero_match.reduce(_ || _)
  val zero_found2 = zero_match2.reduce(_ || _)

  val needle = Reg(UInt(8.W))
  val needle_match = data_bytes.map(_ === needle)
  val needle_found = needle_match.reduce(_ || _)


  val finished = Reg(Bool())    

  

  /*
  順番　
  s_idle
  io.cmd.fire 
  s_acq
  tl.out.a.valid
  tl_out.a.fire
  s_gnt
  s_acq2
  s_acq2
  tl_out.d.ready valid?ready?の違い
  s_resp
  io.resp.fire
  s_idle




  s_idle
  io.cmd.fire 
  s_acq
  tl.out.a.valid
  tl_out.a.fire
  s_gnt
  tl_out.d.ready valid?ready?の違い


  s_acq2
  tl.out.a.valid
  tl_out.a.fire
  s_gnt2

  tl_out.d.ready valid?ready?の違い
  s_resp
  io.resp.fire
  s_idle
  */

  io.cmd.ready := (state === s_idle)

  when(io.cmd.fire){

    printf(p"Fire! Start\n")
    addr := io.cmd.bits.rs1
    addr2 := io.cmd.bits.rs2
    finished := false.B
    ret := 0.U
    resp_rd := io.cmd.bits.inst.rd
    state := s_acq
    printf(p"state is $state\n")
    printf(p"\n")

  }

  val (tl_out, edgesOut) = outer.atlNode.out(0)
  tl_out.a.valid := (state === s_acq || state === s_acq2)
  tl_out.a.bits := edgesOut.Get(
                      fromSource = 0.U,
                      toAddress = addr_block << blockOffset,
                      lgSize = lgCacheBlockBytes.U)._2


  when (tl_out.a.fire) {
    printf(p"Got tl_out.a!\n")
    state := Mux(state === s_acq, s_gnt, s_gnt2)
    printf(p"state is $state\n")
    printf(p"\n")

  }
  
  tl_out.d.ready := (state === s_gnt || state === s_gnt2)
  val gnt = tl_out.d.bits

  when (tl_out.d.fire) {
    printf(p"Got tl_out.d.fire!\n")
    printf(p"state is $state\n")

    when(state === s_gnt){
      printf(p"recv_beat + 1.U, recv_data := gnt.data\n")
      recv_beat := recv_beat + 1.U
      recv_data := gnt.data
      state := s_acq2
      printf(p"state is $state\n")

    } .elsewhen(state === s_gnt2){
      printf(p"recv_beat2 + 1.U, recv_data2 := gnt.data\n")
      recv_beat2 := recv_beat2 + 1.U
      recv_data2 := gnt.data
      state := s_process
      printf(p"state is $state\n")
    }
    printf("gnt.data = %b\n", gnt.data)

    printf(p"\n")

  }


  when(state === s_process){
    printf(p"state is $state\n")


    //indexいるの？
    // needle = data_bytes(index)
    // index := index + 1.U


    //for文を利用せずに、next_boolを外のwireにすることはできないのかな？
    for(i <- 0 until bool_table.length - 1){
        bool_table_next(i + 1) := Mux(data_bytes2(i) === needle, bool_table(i), false.B)

        printf("")
        printf("data_bytes = %c\n", data_bytes(i))//print character 
        printf("data_bytes2 = %c\n", data_bytes2(i))//print character 

        printf(p"0 === ${data_bytes(i) === 0.U}\n")
        printf(p"0 === ${data_bytes2(i) === 0.U}\n")

    }

    bool_table := bool_table_next
    state := s_check
  }

  when(state === s_check){
    printf(p"state is $state\n")
    when(!needle_found){
      printf(p"needle not found!\n")
      // index := 0.U
      state := s_gnt2
    }.otherwise{
      state := s_process
      // state := s_resp
      printf(p"needle found!\n")

    }

    when(zero_found){
      printf(p"zero_found ON!")
      // when(bool_table(cacheDataBeats)){ //when word was cut halfway
      //     state := s_conti_process
      // }
      when(bool_table.slice(0, cacheDataBeats - 1).reduce(_ || _)){//word was found. Go to response
          finished := true.B
          // addr_count := index
      }
    }

    when(zero_found2){
      printf(p"zero_found2 ON!\n")

      finished := true.B
    }// sentence reached \0. Word was not found. Go to response

    when(recv_beat === cacheDataBeats.U){
        printf(p"recv_beat === cacheDataBeats.U ON!")
        recv_beat := 0.U
        addr := next_addr
        state := Mux(recv_beat2 === cacheDataBeats.U, s_resp, s_acq2)

    } .elsewhen(recv_beat2 === cacheDataBeats.U){
        printf(p"recv_beat2 === cacheDataBeats.U ON!")
        recv_beat2 := 0.U
        addr2 := next_addr2
        state := Mux(finished, s_resp, s_acq)
    }
    printf(p"state is $state\n")
    printf(p"finished is $finished\n")
    printf(p"\n")

  }

// Response Here
  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := ret

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
    
  }

//これが必要か確認
  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := false.B
  tl_out.b.ready := true.B
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
  override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("aCustomTileLink")))))

}

class aCustomAccelTestImp(outer: aCustomTestAccel)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) 
  with HasCoreParameters 
  with HasL1CacheParameters{


//TEST
  val cacheParams = tileParams.dcache.get

  val s_idle :: s_gnt :: s_acq :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val resp_rd = Reg(chiselTypeOf(io.resp.bits.rd))
  val ret = Reg(UInt(xLen.W))

  // val addr = RegInit(0.U(coreMaxAddrBits.W))

  private val blockOffset = blockOffBits
  private val beatOffset = log2Up(cacheDataBits/8)


  val addr = RegInit(0.U(coreMaxAddrBits.W))
  val addr2 = RegInit(0.U(coreMaxAddrBits.W))

  val offset = addr(blockOffset - 1, 0)
  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)
  


  io.cmd.ready := (state === s_idle)

  when(io.cmd.fire){
    addr := io.cmd.bits.rs1
    addr2 := io.cmd.bits.rs2
    resp_rd := io.cmd.bits.inst.rd
    state := s_resp
    printf(p"Fire! Start\n")
  }

  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd




  when(io.resp.fire){
    printf("addr  = %b\n", addr)//addr1 = 0000000000000000000000000000000001000001
    printf("addr2 = %b\n", addr2)

    printf("resp_rd = %b\n", resp_rd)//addr1 = 01010

    printf("offset = %b\n", offset)
    printf("blockOffset = %b\n", blockOffset.U)
    printf("beatOffset = %b\n", beatOffset.U)

    printf("addr_block << blockOffset = %b\n", addr_block << blockOffset)

    printf("addr_block = %b\n", addr_block)


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

    printf("The end") 
    state := s_idle  
  }

  io.busy := (state =/= s_idle)
  io.mem.req.valid := false.B
  io.interrupt := false.B
  

}

















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



