package EXERCISE2

import scala.annotation.tailrec

// EXERCISE 2.2
object ソートされてるかを調べる {
  // asってなんの略？
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    @tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1) true // 配列の最後まで見たら終わり
      else if (!ordered(as(n), as(n + 1))) false // 2つの要素の並びがorderedにしたがっているか
      else go(n + 1)

    go(0)
}