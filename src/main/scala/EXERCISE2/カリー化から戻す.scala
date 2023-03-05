package EXERCISE2

// EXERCISE 2.4
object カリー化から戻す {
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }
}