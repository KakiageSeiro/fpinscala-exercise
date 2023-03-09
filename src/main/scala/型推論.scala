import EXERCISE3.{Cons, List2}

import scala.annotation.tailrec

// P46 3.3.2 高階関数の型推論の改善
// 書いてあることがいまいち理解できなかったので書いてみた
object 型推論 {
  @tailrec
  def dropWhile[A](list: List2[A], f: A => Boolean): List2[A] = list match {
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => list
  }

  @tailrec
  def dropWhileReverse[A](f: A => Boolean, list: List2[A]): List2[A] = list match {
    case Cons(head, tail) if f(head) => dropWhileReverse(f, tail)
    case _ => list
  }

  // カリー化したよ
  @tailrec
  def dropWhileCurry[A](list: List2[A])(f: A => Boolean): List2[A] = list match {
    case Cons(head, tail) if f(head) => dropWhileCurry(tail)(f)
    case _ => list
  }

  // 引数の順番を逆にしたよ
  @tailrec
  def dropWhileCurryReverse[A](f: A => Boolean)(list: List2[A]): List2[A] = list match {
    case Cons(head, tail) if f(head) => dropWhileCurryReverse(f)(tail)
    case _ => list
  }
}