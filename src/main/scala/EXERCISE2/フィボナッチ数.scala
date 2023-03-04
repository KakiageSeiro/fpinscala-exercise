package EXERCISE2

import scala.annotation.tailrec

// EXERCISE 2.1
object フィボナッチ数 {
  // n番目のフィボナッチ数を返す
  // 0, 1, 1, 2, 3, 5, 8, 13, 21
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, i: Int, 前: Int, カレント: Int): Int = {
      if (n == i) return 前
      go(n, i + 1, カレント, 前 + カレント)
    }

    go(n, 1, 0, 1)
  }

  // ゴールが決まってる & ループカウントを引数に持ちたくなったら、引き算して0をゴールにする。
  def fib2(n: Int): Int = {
    @tailrec
    def go(n: Int, 前: Int, カレント: Int): Int = {
      if (n == 0) return 前
      go(n - 1, カレント, 前 + カレント)
    }

    go(n - 1, 0, 1) // 0をゴールにするときはn - 1からスタート
  }
}