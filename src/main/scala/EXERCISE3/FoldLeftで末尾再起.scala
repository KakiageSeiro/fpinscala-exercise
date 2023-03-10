package EXERCISE3

import scala.annotation.tailrec

// EXERCISE 3.10
object FoldLeftで末尾再起 {
  @tailrec
  def foldLeft[A, B](list: List2[A], z: B)(f: (B, A) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  // EXERCISE 3.11
  def sum(list: List2[Int]): Int = {
    foldLeft(list, 0)(_ + _)
  }

  def product(list: List2[Double]): Double = {
    foldLeft(list, 1.0)(_ * _)
  }

  def length[A](list: List2[A]): Int = {
    foldLeft(list, 0)((acc, _) => acc + 1)
  }

  // EXERCISE 3.12
  def reverse[T](list: List2[T]): List2[T] = {
    foldLeft(list, List2[T]())((acc, head) => Cons(head, acc))
  }

  // EXERCISE 3.13
  // 答えを見たけどよくわからない。以下は理解のために写経したもの。でもわからない。
  def foldRight[A, B](list: List2[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f)) // 末尾再起じゃないことに注意
  }

  // 完成形
  def foldRightを使ったfoldLeft[T, ACC](list: List2[T], z: ACC)(f: (ACC, T) => ACC): ACC = {
    foldRight(list, (b: ACC) => b)((a, g) => b => g(f(b, a)))(z)
  }


  def foldRightを使ったfoldLeft_分解[A, B](list: List2[A], outerIdent: B)(combiner: (B, A) => B): B = {
    // エイリアス
    // https://www.ne.jp/asahi/hishidama/home/tech/scala/type.html
    type BtoB = B => B

    def innerIdent: BtoB = (b: B) => b

    def combinerDelayer: (A, BtoB) => BtoB = (a: A, delayFunc: BtoB) => (b: B) => delayFunc(combiner(b, a))

    def go: BtoB = foldRight(list, innerIdent)(combinerDelayer)

    go(outerIdent)
  }

  // EXERCISE 3.14
  def append[A](list1: List2[A], list2: List2[A]): List2[A] = {
    foldRight(list1, list2)(Cons(_, _))
  }

  def append2[A](list1: List2[A], list2: List2[A]): List2[A] = {
    // こっちはCons(_, _)という渡し方にするとtype mismatchになる。
    // たぶんreverseを経由するとだめ。Consとreverseの型がパラメータではなく同じ型にしないとできなそう。
    foldLeft(reverse(list1), list2)((acc, head) => Cons(head, acc))
  }

  // EXERCISE 3.15
  def flatten[A](list: List2[List2[A]]): List2[A] = {
    // foldRightが分割統治法であることを活かして、ペアずつでappendすればよい。
    // leftだと線形時間にならないことに注意。
    foldRight(list, List2[A]())(append)
  }
}