package EXERCISE8

import EXERCISE6.{RNG, State}
import EXERCISE8.Prop.{FaildCase, SuccessCount}

object Prop {
  type SuccessCount = Int
  type FaildCase = String // 失敗したケースの説明。複数ケースがこけたらどうする？
}

trait Prop {
  self => // traitを実装したインスタンスの自己参照する記法
  def check: Either[(FaildCase, SuccessCount), SuccessCount]

//  def &&(p: Prop): Prop = new Prop {
////    def check = Prop.this.check.map(p.check) match {
////      case Right(value) => Right(value)
////    }
//  }
}

// 思い出し。State[RNG, A]はRNGをつかってAを生成し、次の値を生成するRNGと一緒に返す。
case class Gen[A](sample: State[RNG, A]){
  // EXERCISE 8.6
  // Aに依存したBを生成する君(Gen[B])をfとする
  // 1. sample: State[RNG, A] のflatMapで以下2,3をやる
  // 2. fを適用してGen[B]を得る
  // 3. Gen[B]のsampleであるState[RNG, A]を得る
  // 4. State[RNG, A]をGenで包む
  //
  // Q. つまりなにがしたかったの？
  // A. Genの中のStateのAをBにしたい
  //     なぜしたいのか？
  //     Bが「Aに依存した生成方法の結果型」であることを思い出す
  //     この関数が生まれたそもそもの目的「Gen[(A,B)]を作る」を思い出す
  //     この関数があることで、Gen[A]と引数fがあれば、Gen[B]が作れる
  //     なのでGen[(A,B)]が作れるようになる
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  // 引数にIntを取る。↓のlistOfNはGen[Int]を取るので、文脈(Gen)をflatMapで外したIntを取れば、objectに定義してあるlistOfNを使える。
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(sizeを生成君: Gen[Int]): Gen[List[A]] =
    sizeを生成君.flatMap(生成したsize => this.listOfN(生成したsize))
}

object Gen {
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  // EXERCISE 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // EXERCISE 8.5
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  // 引数nの数の、生成された値AのListをもつGenを作る
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  // EXERCISE 8.7
  // "2つのGenを結合"
  // "結合は、各ジェネレータから同じ値になる可能性があるものを取り出すという方法で行う"
  // g1,g2の結果で重複を排除する？と考えたが、ランダムの値を生成するGenなのに重複排除とは？となり答え見た。見ても何がしたいのかわからない。
  // 答えの実装は2分の1の確立でどちらかのGenが選ばれる実装。問題文と答えが自分の頭の中で紐付けられない…
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  // EXERCISE 8.8
  // これもよく分からず答えを見た。
  // unionは2分の1だったが、こちらはg1の結果によってどちらが選ばれるかが変わる。
  // そもそも「どちらかから選ぶ」ということがよく分かっていない。「最初に生成したAに依存するBを作る話」だったと思うが「ランダムでなにか分岐したい」の話に変わっている？
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }


  // ■■■この章なんもわからん挫折！！次の章にいくぞ！！
}
