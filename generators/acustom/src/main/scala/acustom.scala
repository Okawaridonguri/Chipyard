package acustom

import chisel3._
import chisel3.util.Enum

// import org.chipsalliance.cde.config.{Config}
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._ 

import freechips.rocketchip.tilelink._

// import freechips.rocketchip.util._ //log2Up 用

import chisel3.util.log2Up

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

class aCustomAccel(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes = opcodes){
  override lazy val module = new aCustomAccelImp(this)
  override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("aCustomTileLink")))))

  // override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("CharacterCountRoCC")))))
}


class aCustomAccelImp(outer: aCustomAccel)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) 
  with HasCoreParameters 
  with HasL1CacheParameters{

  val cacheParams = tileParams.dcache.get

  val s_idle  :: s_acq :: s_gnt :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val resp_rd = Reg(chiselTypeOf(io.resp.bits.rd))
  val retur = Reg(UInt(xLen.W))


//addr計算
  private val blockOffset = blockOffBits
  private val beatOffset = log2Up(cacheDataBits/8)

  val addr = RegInit(0.U(coreMaxAddrBits.W))
  val offset = addr(blockOffset - 1, 0)
  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)


  io.cmd.ready := (state === s_idle)

  when(io.cmd.fire){
    addr := io.cmd.bits.rs1
    printf("rd = %b\n", addr)
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

  when (tl_out.a.fire){ 
    state := s_gnt 
    printf(p"Got tl_out.a!\n")
  }
  
  tl_out.d.ready := (state === s_gnt)
  val gnt = tl_out.d.bits

  when (tl_out.d.fire) {
    state := s_resp
  }

  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := retur

  when(io.resp.fire){
    printf("addr1 = %b\n", addr)

    printf("gnt.data = %b\n", gnt.data)

    printf("The end") 
    state := s_idle  
  }

  io.busy := (state =/= s_idle)
  io.interrupt := false.B
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
  val retur = Reg(UInt(xLen.W))

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



