package EXERCISE3

// EXERCISE 3.9
object FoldRightを使ってリストの長さを計算 {
  private def foldRight[A, B](list: List2[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f)) // 末尾再起じゃないことに注意
  }

  // accはaccumulatorの略で、蓄積器とか累算器という意味みたい。
  def length[A](list: List2[A]): Int = {
    foldRight(list, 0)((_, acc) => acc + 1)
  }
}