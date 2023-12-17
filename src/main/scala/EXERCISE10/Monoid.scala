package EXERCISE10

import EXERCISE8.Gen.forAll
import EXERCISE8.{Gen, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

// op(op(x, y), z) == op(x, op(y, z)) が成り立つ
// op(x, zero) == xとop(zero, x) == xが成り立つ





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

  override def zero = (a : A) => a
}

// EXERCISE 10.4
// 答え見た。Propの関数forAllででテストを実行してテスト結果をPropに格納したものを返している
def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  // Associativity
  forAll(for {
    x <- gen
    y <- gen
    z <- gen
  } yield (x, y, z))(p =>
    m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
    // Identity
    forAll(gen)((a: A) =>
      m.op(a, m.zero) == a && m.op(m.zero, a) == a)

// AのMonoidを作れば、foldLeftとRightができる
def concatenate[A](as: List[A], m: Monoid[A]): A =
  as.foldLeft(m.zero)(m.op)

// EXERCISE 10.5
// モノイドにおける結合をやるまえにmapの関数を適用するやつ。map適用後に結合するのでmはBである必要がある
def foldMap[A, B](as: List[A], m:Monoid[B])(f: A => B) = {
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




// We can get the dual of any monoid just by flipping the `op`.
def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
  def op(x: A, y: A): A = m.op(y, x)
  val zero = m.zero
}












