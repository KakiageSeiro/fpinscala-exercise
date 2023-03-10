package EXERCISE3

// EXERCISE 3.8
object Consを引数に取るFoldRight {
  def foldRight[A, B](list: List2[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f)) // 末尾再起じゃないことに注意
  }
}