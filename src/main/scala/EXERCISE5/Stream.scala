package EXERCISE5

trait Stream[+A] {
  // ~~これ戻り値のAがコンパイルエラーになる。headOption[A]にするとSome[Any]を戻り値にしないといけなくなる)~~
  // ↓
  // 最初はこの関数をobject Streamの中に書いていたが、objectではコンパニオンだけにして、thisを使うような処理は全部traitに入れてしまえば良い。
  // traitをJavaのinterfaceと理解していると、interfaceのデフォルト実装のように思ってしまうが、そうではなく「Javaでのinterface "もしくは" abstract classのようなもの」という認識で良い。
  // そう理解すると、case classやcase objectで複数の構造を取りつつ、それらを一つの場所(一つの概念)(いままでつかってたデータとそれを扱う処理としてのメソッドの集合であるclass)として扱えるような構文であるということが理解でき、traitを作りたい動機が自分の中から沸いてくる。
  def headOption: Option[A] = {
    println("headOption:start")

    val result: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => {
        // hは() => Aなので()をつけることで評価されAになるのでSome[A]が返ると思うが、Option[() => Int]が返る。なぜ？
        // hに{ println("よびだし1"); () => 1 }を渡していた。{ println("よびだし1"); 1 }を渡せばOption[Int]が返る。
        val hh = h()
        Some(hh)
      }
    }

    println("headOption:end")
    result
  }

  // EXERCISE 5.1
  def toList: List[A] =  {
    println("toList:start")

    val result = this match {
      case Empty => List()
      // hとtは"すぐに評価されない形式の式"であるサンクなので、()をつけて評価する
      case Cons(h, t) => {
        println("toList:処理中のhead" + h())
        h() :: t().toList
      }
    }

    println("toList:end")

    result
  }

  // EXERCISE 5.2 その1/2
  def take(n: Int): Stream[A] = {
    println("take:start")

    if (n == 0) return Stream.empty

    val result = this match {
      case Empty => Stream.empty
      case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
    }

    println("take:end")

    result
  }

  // EXERCISE 5.2 その2/2
  def drop(n: Int): Stream[A] = {
    println("drop:start")

    if (n == 0) return this

    val result = this match {
      case Empty => Stream.empty
      case Cons(_, t) => t().drop(n - 1)
    }

    println("drop:end")

    result
  }

  // EXERCISE 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    println("takeWhile:start")

    val result = this match {
      case Empty => Stream.empty
      case Cons(h, t) => {
        if (p(h())) Stream.cons(h(), t().takeWhile(p))
        else Stream.empty
      }
    }

    println("takeWhile:end")

    result
  }
}







case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    println("Stream.cons")
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = {
    println("Stream.empty")
    Empty
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
