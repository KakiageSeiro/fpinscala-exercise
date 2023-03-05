package EXERCISE3

// EXERCISE 3.2
object 先頭以外の要素を取る {
  def tail[A](list: List2[A]): List2[A] = list match{
    case Cons(_, tail) => tail
  }
}