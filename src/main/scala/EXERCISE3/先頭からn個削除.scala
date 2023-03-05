package EXERCISE3

import scala.annotation.tailrec

// EXERCISE 3.4
object 先頭からn個削除 {
  @tailrec
  def drop[A](list: List2[A], n: Int): List2[A] = {
    if (n == 0) return list

    list match {
      case Cons(_, tail) => drop(tail, n - 1)
    }
  }
}