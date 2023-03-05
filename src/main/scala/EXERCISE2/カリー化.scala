package EXERCISE2

// EXERCISE 2.3
object カリー化 {
  // 戻り値がA => B => C じゃなくて括弧ついてる理由はなんだろう
  // 右結合だからとのこと。
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }
}