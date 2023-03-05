package EXERCISE3

import scala.annotation.tailrec

// EXERCISE 3.5
object 条件にマッチするところまで削除 {
  @tailrec
  def dropWhile[A](list: List2[A], f: A => Boolean): List2[A] = list match {
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => list
  }
}