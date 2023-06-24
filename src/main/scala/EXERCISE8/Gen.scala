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

  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
}

// 思い出し。State[RNG, A]はRNGをつかってAを生成し、次の値を生成するRNGと一緒に返す。
case class Gen[A](sample: State[RNG, A])

object Gen {
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  // EXERCISE 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // EXERCISE 8.5
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

}
