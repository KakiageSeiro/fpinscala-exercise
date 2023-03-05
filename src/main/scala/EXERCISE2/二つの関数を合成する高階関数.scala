package EXERCISE2

// EXERCISE 2.5
object 二つの関数を合成する高階関数 {
  def compose[A,B,C](f: B => C, g: A=> B): A => C = {
    a => f(g(a))
  }
}