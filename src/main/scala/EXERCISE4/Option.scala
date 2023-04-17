package EXERCISE4

import EXERCISE4.Option.None.map2

object Option {
  // EXERCISE 4.1
  // 基本的に問題が言ってることがわからない。翻訳のアレかも。というわけで関数名から推測した実装にする。
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }

    // EXERCISE 4.3
    // 2つのOptionを使ってfを適用するmap
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a match {
      case None => None
      case Some(a) => b.map(bb => f(a, bb)) // aもbもSomeだったら、fする。
      // 複数のOptionをつかって何かの処理をしたいときは、mapを数珠繋ぎにして全部とり出してからfすれば、何かがNoneのときに実行しない。
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(x) => f(x)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case Some(x) => Some(x)
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(x) if f(x) => Some(x)
      case _ => None // ↑のifがfalseの時用
    }
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  // 平均
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // EXERCISE 4.2
  // 分散
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap( // Optionで返すのでflatMap
      xsの平均 => mean(xs.map(x => math.pow(x - xsの平均, 2)))) // 分散 = math.pow(x - m, 2)の平均
  }

  // EXERCISE 4.4
  // 答え見た。
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
    // 途中の要素がNoneだった場合、hにNoneが入り、flatMapでNoneがreturnされる
    // 呼出し元の再起sequenceからNoneが返ってくるので、後続mapが動かずそのままNoneがreturnし、再起の最初までNoneが返る。
    // この問題は要素を一つずつみる事と、そのときにNoneだったらmapが動かない性質を利用することを思いつく必要があったっぽい。
  }

  // EXERCISE 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case head :: tail => map2(f(head), traverse(tail)(f))(_ :: _)
  }

  // 2つflatMapしたい時はfor使ったほうが見やすいかも
  def traverse_for[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    if (a.isEmpty) return Some(Nil)

    for {
      head <- f(a.head)
      tail <- traverse(a.tail)(f)
    } yield head :: tail
  }
}