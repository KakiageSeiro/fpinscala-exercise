package EXERCISE3

import EXERCISE3.FoldLeftで末尾再起.append

import scala.annotation.tailrec

object リストを扱うその他の関数 {
  private def foldRight[A, B](list: List2[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f)) // 末尾再起じゃないことに注意
  }

  @tailrec
  private def foldLeft[A, B](list: List2[A], z: B)(f: (B, A) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  // EXERCISE 3.16
  def addOne(list: List2[Int]): List2[Int] = {
    foldRight(list, Nil: List2[Int])((head, tail) => Cons(head + 1, tail))
  }

  // EXERCISE 3.17
  def doubleToString(list: List2[Double]): List2[String] = {
    foldRight(list, Nil: List2[String])((head, tail) => Cons(head.toString, tail))
  }

  // EXERCISE 3.18
  def map[A, B](list: List2[A])(f: A => B): List2[B] = {
    foldRight(list, Nil: List2[B])((head, tail) => Cons(f(head), tail))
  }

  // EXERCISE 3.19
  def filter[A](list: List2[A])(f: A => Boolean): List2[A] = {
    val フィルター = (head:A , tail:List2[A]) => {
      if (f(head)) Cons(head, tail)
      else tail
    }
    foldRight(list, Nil: List2[A])((head, tail) => フィルター(head, tail))
  }

  // EXERCISE 3.20
  def flatMap[A, B](list: List2[A])(f: A => List2[B]): List2[B] = {
    foldRight(list, Nil: List2[B])((head, tail) => append(f(head), tail))
  }

  // EXERCISE 3.21
  def flatMapFilter[A](list: List2[A])(f: A => Boolean): List2[A] = {
    val フィルター = (head:A , tail:List2[A]) => {
      if (f(head)) Cons(head, tail)
      else tail
    }
    flatMap(list)(head => フィルター(head, Nil))
  }

  // EXERCISE 3.22
  def zip(list1: List2[Int], list2: List2[Int]): List2[Int] = {
    (list1, list2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(head1 + head2, zip(tail1, tail2))
    }
  }

  // EXERCISE 3.23
  def zipWith(list1: List2[Int], list2: List2[Int])(f: (Int, Int) => Int): List2[Int] = {
    (list1, list2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(f(head1, head2), zipWith(tail1, tail2)(f))
    }
  }

  def scanListを試す(list: List[Int]): List[Int] = {
    // 累積和かんたん！
    list.scanLeft(0)(_ + _)
  }

  def scanRightを試す(list: List[Int]): List[Int] = {
    // 計算途中の結果を出してくれる。つまり計算途中のaccumulatorの値を出してくれる。
    list.scanRight(0)(_ + _)
  }
}