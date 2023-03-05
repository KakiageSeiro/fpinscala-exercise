package EXERCISE3

// EXERCISE 3.3
object 先頭を別の要素に置き換える {
  def setHead[A](list: List2[A], setElement: A): List2[A] = list match{
    case Cons(_, tail) => Cons(setElement, tail)
  }
}