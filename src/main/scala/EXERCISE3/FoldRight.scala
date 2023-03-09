package EXERCISE3

// EXERCISE 3.7
object FoldRight {
  // "0.0を検出した場合に、直ちに再起を中止して0.0を返せるか"問題
  //   もともとはfの戻り値を型パラメータBで指定してたけど、0.0の固定値を返すためにDoubleにした。これをしない場合はAnyにしないと無理そう。
  private def foldRightDouble[A](list: List2[A], z: Double)(f: (A, Double) => Double): Double = list match {
    case Nil => z
    case Cons(0.0, _) => 0.0
    case Cons(head, tail) => f(head, foldRightDouble(tail, z)(f)) // 末尾再起じゃないことに注意
  }

  def product2(list: List2[Double]): Double = {
    foldRightDouble(list, 1.0)(_ * _)
  }
}