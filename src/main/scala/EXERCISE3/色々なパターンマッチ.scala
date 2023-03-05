package EXERCISE3

// EXERCISE 3.1
sealed trait List2[+A]
case object Nil extends List2[Nothing]
case class Cons[+A](head: A, tail: List2[A]) extends List2[A]

object List2{
  def sum(ints: List2[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List2[Double]): Double = ds match{
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List2[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}

object 色々なパターンマッチ {
  def どれになる(): Int = {
    val x = List2(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List2.sum(t)
      case _ => 101
    }
    x
  }
}