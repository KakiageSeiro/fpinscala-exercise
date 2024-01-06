package EXERCISE10

import EXERCISE10.WordCount.wordCountMonoid
import EXERCISE10.色々なモノイド.{foldMapV, intAddition, stringMonoid}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

// op(op(x, y), z) == op(x, op(y, z)) が成り立つ
// op(x, zero) == xとop(zero, x) == xが成り立つ

object 色々なモノイド {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    def zero: List[A] = Nil
  }

  // EXERCISE 10.1
  // intのたし算
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero: Int = 0
  }
  // intのかけ算
  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero: Boolean = true
  }

  // モノイドの定義を満たしているか確認する
  // op(op(x, y), z) == op(x, op(y, z)) が成り立つ
  // op(x, zero) == xとop(zero, x) == xが成り立つ
  //
  // x = Some(1)
  // y = Some(2)
  // z = Some(3)
  // とする
  // op(op(x, y), z) == op(op(Some(1), Some(2)), Some(3)) == Some(1)
  // op(x, op(y, z)) == op(Some(1), op(Some(2), Some(3))) == Some(1)
  // op(x, zero) == op(Some(1), Option.empty) == Some(1)
  // op(zero, x) == op(Option.empty, Some(1)) == Some(1)
  // EXERCISE 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2

    def zero = Option.empty
  }

  // EXERCISE 10.3
  // 引数と戻り値の型が同じ関数をendo関数と呼ぶ
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A) = a1 compose a2

    override def zero = (a: A) => a
  }

  // EXERCISE 10.4
  // 答え見た。Propの関数forAllででテストを実行してテスト結果をPropに格納したものを返している
  // Part2は飛ばした箇所があるので、その部分を利用するためコンパイルエラーになる
  //def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  //  // Associativity
  //  forAll(for {
  //    x <- gen
  //    y <- gen
  //    z <- gen
  //  } yield (x, y, z))(p =>
  //    m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
  //    // Identity
  //    forAll(gen)((a: A) =>
  //      m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  // AのMonoidを作れば、foldLeftとRightができる
  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // EXERCISE 10.5
  // モノイドにおける結合をやるまえにmapの関数を適用するやつ。map適用後に結合するのでmはBである必要がある
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B) = {
    as.map(f).foldLeft(m.zero)(m.op)
    // 答えでは as.foldLeft(m.zero)((b, a) => m.op(b, f(a))) となっている
    // 左畳み込みなのでfoldLeftの第2引数は(b, a)のように、左がbである。第1引数がm.zeroでmはBである。そのため、右のaにだけm.opでBに変換すれば良い
  }

  // EXERCISE 10.6
  // 答え見た。B->Bにして結合しまくって最後に初期値zをあげて計算を実行するという発想によって生まれるモノだと思うけど、自分で思いつける気がしない。
  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val aToBToB: A => B => B = f.curried
    val bToB: B => B = foldMap(as, endoMonoid[B])(aToBToB) // foldMapはBを返すが、Bはfの戻り値型のことなのでB=>B(を結合しまくったやつ)という型がfoldMapにとってのBになる
    bToB(z)
  }

  //
  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  // 反転という発想はわかるけど、反転させても上手くいくかどうかをどうやって確信しているんだろう。モノイドって左から右という暗黙ルールがあるようにみえるので反転させることで不安になる。実際にモノイドのルールに反転させて適用させてみればいいのか
  // モノイドのルール
  // op(op(x, y), z) == op(x, op(y, z)) が成り立つ
  // op(x, zero) == xとop(zero, x) == xが成り立つ
  // x = 1, y = 2, z = 3 たし算とする
  // opの引数部分は反転させる
  // op(3, op(2, 1)) == 6
  // op(op(3, 2), 1) == 6
  // op(0, 1) == 1
  // op(1, 0) == 1
  //
  // おわー反転しても全然大丈夫じゃん。でもまだ不安なのでOptionでやる
  // x = Some(1)
  // y = Option.empty
  // z = Some(3)
  // とする
  // 反転前はこう
  // op(op(x, y), z) == op(op(Some(1), Option.empty), Some(3)) == Some(1)
  // op(x, op(y, z)) == op(Some(1), op(Option.empty, Some(3))) == Some(1)
  // op(x, zero) == op(Some(1), Option.empty) == Some(1)
  // op(zero, x) == op(Option.empty, Some(1)) == Some(1)
  // 反転後
  // op(Some(3), op(Option.empty, Some(1))) == Some(3) // ■ここ3になってる
  // op(op(Some(3), Option.empty), Some(1)) == Some(3) // ■ここ3になってる
  // op(Option.empty, Some(1)) == Some(1)
  // op(Some(1), Option.empty) == Some(1)
  //
  // モノイドのルールは満たしているが、このfoldLeftの実装では結果が変わってしまうのでは？左からたし算などのopする分にはいいけど、順序がかわるのはやっぱりだめでは？
  //
  // とおもったが、Same(3)の場所がかわるのはちがうのでは？変化してもよいのはopの位置だけで、各要素を入れ替えて良いわけではない
  // つまり、反転前の1番目の2番目がopの位置を変化させたもの。3番目と4番目がopの位置を変化させたもの。というペアになっている。そしてこのペアは結果が変化しない
  // なのでモノイドのルール1を満たしている。ルール2は単位元を適用しても変化しないことを確認する意図であり、要素を入れ替えるのはルール2の話であってルール1はちがう。
  // このことから、foldLeftとRightで結果が変化しない(opを適用する場所を変化(左か右か)させるのはルール1をやっているだけなので)


  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  // EXERCISE 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.length match {
    case 0 => m.zero // 単位元をopしても変化がないことをがわかっていると、再起処理の終了条件をビビらず書けるぜ！これによって2分した余りにもビビらずによくなってるぜ！
    case 1 => f(v(0)) // foldMapのMap部分
    case _ => // foldMapVのV部分?Vって分割することを意味してるよね？単語を略すぐらいなら"foldMapを大体log nでやる良い感じの関数"という関数名にしろ！(過激)
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
  }
}

// EXERCISE 10.8
// 答え見た。実装してないやつだっけ？コンパイルエラーになる。結局コンビネータみたいな概念？をよくわかってないのでこの実装をみてもよくわからない
// This ability to 'lift' a monoid any monoid to operate within
// some context (here `Par`) is something we'll discuss more in
// chapters 11 & 12
//
//def par[A](m: Monoid[A]): Monoid[Par[A]] = new: // この文法なに？
//def combine[A](a1: Par[A], a2: Par[A]) = a1.map2(a2)(m.combine)
//val empty = Par.unit(m.empty)
//
//// we perform the mapping and the reducing both in parallel
//def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
//  Par.parMap(as)(f).flatMap(bs =>
//    foldMapV(bs, par(m))(b => Par.lazyUnit(b))
//  )

// EXERCISE 10.9
// 答え見た。わからん。なんか順序を判定してそうな雰囲気はわかるが、処理の説明が出来ないと思う。
//
//val orderedMonoid: Monoid[(Boolean, Int)] = new:
//def combine(a1: (Boolean, Int), a2: (Boolean, Int)) =
//  (a1(0) && a2(0) && a1(1) <= a2(1), a1(1) max a2(1))
//val empty = (true, Int.MinValue) // 単位元のこと。なんでここまではzeroという名前をつかってたのにemptyになるんだ
//
//def ordered(ints: IndexedSeq[Int]): Boolean =
//  foldMapV(ints, orderedMonoid)(i => (true, i))(0)


sealed trait WordCount // 書籍ではWCという名称

case class Stub(chars: String) extends WordCount // 単語として検出指定ない状態の文字列

case class Part(leftStub: String, wordsCount: Int, rightStub: String) extends WordCount // 単語として検出した状態の文字列の数をwordCountにし、左と右のStubを持つ

// EXERCISE 10.10
// 答え見た。case classで複数ありうる場合はすべての組み合わせを列挙してそれを足し合わせる(opをやる)方法を書けば良かったんだな
// Part同士をopする場合、Partになっている時点でLeft,RightはStub(まだスペースを検出していない文字列)なので、PartとPartの間の文字(Part1のRightとPart2のLeft)が空白以外だったらwordCountに加算してしまって良いという発想がでてこなかった。良く思いつけるな
object WordCount {

  val wordCountMonoid = new Monoid[WordCount] {
    def op(a1: WordCount, a2: WordCount): WordCount = (a1, a2) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(l, w, r)) => Part(a + l, w, r)
      case (Part(l, w, r), Stub(b)) => Part(l, w, r + b)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(
          l1,
          w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2,
          r2
        )
    }

    def zero: WordCount = Stub("")
  }

  // EXERCISE 10.11
  def wordCount(target: String, monoid: Monoid[WordCount]): Int = {
    wordCountGo(target, monoid) match {
      case Stub(_) => 0
      case Part(_, wordsCount, _) => wordsCount
    }
  }

  def wordCountGo(target: String, monoid: Monoid[WordCount]): WordCount = target match {
    case "" => Stub("")
    case _ =>
      val (l, r) = target.splitAt(target.length / 2)
      monoid.op(wordCountGo(l, monoid), wordCountGo(r, monoid))
  }
}

// EXERCISE 10.12
// StreamとTreeがなんだったか完全にわすれたので飛ばす
trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(mb: Monoid[A]): A
}

object FoldableList extends Foldable[List] {
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: List[A])(mb: Monoid[A]): A = as.foldRight(mb.zero)((a, b) => mb.op(a, b))
}

object FoldableIndexedSeq extends Foldable[IndexedSeq] {
  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)

  def concatenate[A](as: IndexedSeq[A])(mb: Monoid[A]): A = as.foldRight(mb.zero)((a, b) => mb.op(a, b))
}

// EXERCISE 10.14
object FoldableOption extends Foldable[Option] {
  def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Some(a) => f(a)
    case None => mb.zero
  }

  def concatenate[A](as: Option[A])(mb: Monoid[A]): A = as.foldRight(mb.zero)((a, b) => mb.op(a, b))
}

// EXERCISE 10.15
// Fの部分がコンパイルエラーになってしまうので答えを見た
//object FodlableToList extends Foldable[_] {
//  def toList[A](fa: F[A]): List[A]
//}
//
// implicit classに相当するScala3の構文extensionを使ってTraitを拡張する方法で、Listの場合だけ該当するTrait?を定義できるみたい
// 以下は答え丸写し
// trait Foldable[F[_]]:
//  extension [A](as: F[A])
//    def toList: List[A] =
//      as.foldRight(List.empty[A])(_ :: _)
//
//object Foldable:
//  given Foldable[List] with
//    extension [A](as: List[A])
//      override def foldRight[B](acc: B)(f: (A, B) => B) =
//        as.foldRight(acc)(f)
//      override def foldLeft[B](acc: B)(f: (B, A) => B) =
//        as.foldLeft(acc)(f)
//      override def toList: List[A] = as

// EXERCISE 10.16
// いままで作っていたStringMonoidのように具体的な型ではなく、型パラメータを取るMonoidの定義なので
// opの中でAとBのMonoid.opを使う必要がある(A、Bの足し方を知っている必要がある)ので、引数にMonoidを受け取る
object Monoid {
  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)): (A, B) =
        (ma.op(x._1, y._1), mb.op(x._2, y._2))

      val zero: (A, B) = (ma.zero, mb.zero)
    }

  // こんな感じで使う
  val productMonoidInstance: Monoid[(String, WordCount)] = Monoid.productMonoid(stringMonoid, wordCountMonoid)
}


// EXERCISE 10.17
object functionMonoid {
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B) = (a: A) => B.op(a1(a), a2(a))

      override def zero = (_: A) => B.zero
    }
}

// 期待する動作
// bag(Vector("a", "rose", "is", "a", "rose")
// res0: Map[String, Int] = Map(a -> 2, rose -> 2, is -> 1)
object bagMonoidObject {
  // EXERCISE 10.18
  // この問題は①IndexedSeqの要素をMapに変換する部分②Mapをモノイドで操作する部分。の2つがあるのでわかりにくい。
  // ①bag関数の最初でIndexedSeqからMap変換をやる
  // ②モノイドを利用する部分はMapなので、VectorではなくMap[A, Int]でモノイドを作る
  def bagMonoid[A]: Monoid[Map[A, Int]] =
    new Monoid[Map[A, Int]] {
      def op(m1: Map[A, Int], m2: Map[A, Int]): Map[A, Int] =
        (m1.keySet ++ m2.keySet).foldLeft(zero) { (acc, key) =>
          acc.updated(key, m1.getOrElse(key, 0) + m2.getOrElse(key, 0))
        }

      def zero: Map[A, Int] = Map.empty
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    // ふつうにやるならこう
    // as.groupBy(identity).view.mapValues(_.size).toMap
    //
    // groupby().mapValuesにしようとしたが非推奨になってて、view.mapValuesを使うようにコメントがあった。
    // viewは遅延評価用の型であるMapViewを返すので最後にtoMapで正格する必要がある

    // モノイドをつかう方法
    as.map(a => Map(a -> 1)).foldLeft(bagMonoid[A].zero)(bagMonoid[A].op)
  }
}


object 畳み込みする時に複数の計算ができる {
  private val 左はcount右はsumにつかうMonoid: Monoid[(Int, Int)] = Monoid.productMonoid(intAddition, intAddition)

  // def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))
  // (a => (1, a))の部分で(1, 1)(1, 2)(1, 3)(1, 4)をつくる。タプルの左はカウント用、右はsum用
  // このように、タプルで複数の値を作り、Monoid[(Int, Int)]のように複数の値をあつかうMonoidに流すことで、Listを一度の読み込みで複数種類の計算ができる
  private val count_sum = FoldableList.foldMap(List(1, 2, 3, 4))(a => (1, a))(左はcount右はsumにつかうMonoid)

  private val ave = count_sum._2 / count_sum._1 // 2.5
}

























