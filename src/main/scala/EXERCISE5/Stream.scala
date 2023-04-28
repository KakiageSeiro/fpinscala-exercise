package EXERCISE5

import EXERCISE5.Stream.cons

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

  // EXERCISE 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().forAll(p)
    case _ => false
  }

  // EXERCISE 5.5
  def foldRight[B](z : => B)(f: (A, => B) => B): B = this match{
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // 答え見た。foldRightの第二引数fの引数が(A, => B)でぱっと見混乱するが(A, () => B)と同じ。
  // (A, => B)この構造がListのhead,tail(Consの(h: () => A, t: () => Stream[A])も同じ)の構造と同じであることに気づく問題だったと思う
  def takeWhile_foldRight(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else Stream.empty)
  }

  // EXERCISE 5.6
  // これが難問とされてるのはなんでだ
  def headOption_foldRight: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }

  // EXERCISE 5.7
  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((h, t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, t) => {
      if (f(h)) cons(h, t)
      else t
    })
  }

  // これではcons()の戻り値がStream[Any]になるエラーでコンパイルできない
//  def append(a: () => Stream[A]): Stream[A] = {
//    foldRight(a())((h, t) => cons(h, t))
//  }

  // 答え見た。[B >: A]というのはBはAの親。何回みてもパッとわからない。
  // appendするのにBという型が登場するのが理解できない。ので型パラメータを書くという発想自体できなかった。
  // appendする対象(末尾に追加する方)がスーパータイプであることを想定しなければいけないのは何で？
  // また、スーパータイプを登場させずにappendを実装する方法はないのか？
  // ちなみに、コレクションの定義↓
  // trait Collection[A] {
  //  def appended[B >: A](b: B): Collection[B]
  //  def :+[B >: A](b: B): Collection[B]
  //}
  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h).append(t))


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


  // EXERCISE 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val 無限a: Stream[A] = Stream.cons(a, 無限a)
    無限a
  }

  // EXERCISE 5.9
  def from(n: Int):Stream[Int] =
    Stream.cons(n, from(n + 1))

  // EXERCISE 5.10
  def fibs(): Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = {
      Stream.cons(n1, go(n2, n1 + n2))
    }

    go(0, 1)
  }

  // EXERCISE 5.11
  // 答え見た。問題文が言いたいことがわからなかった。シグネチャを略して英単語１文字にする文化が悪い。
  def unfold[A, S](初期状態: S)(現在状態Aと次の値Sを生成するf: S => Option[(A, S)]): Stream[A] =
    現在状態Aと次の値Sを生成するf(初期状態) match {
      case Some((a, s)) => cons(a, unfold(s)(現在状態Aと次の値Sを生成するf))
      case None => empty
    }



}
