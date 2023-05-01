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
  def toList: List[A] = {
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
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
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

  // EXERCISE 5.13
  // 答え見た。タプルを一つの型として渡す発想がでてこない。
  // fの戻り値のOption[A,S]のSは、次のzにすればよい。ということもわかってなかった。
  // この実装を見た時にループする部分がどこかわからなかった。
  // ループはunfoldの実装の中で再起していて、OptionがNoneになるまでやる。
  // なのでunfoldはfor(i = 0; i < 10; i++)の2番目と3番目を書くのと同じように使えるということに今まで気付かなかった。
  // 2番目の終了条件はなくてもよい。呼出し側がtakeなどをつかうことを期待しゆだねることができる。
  def take_unfold(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (Stream.empty, 0))) // 終了条件
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1))) // forの3番目
      case _ => None
    }
  }

  def takeWhile_unfold(f: A => Boolean): Stream[A] = {
    Stream.unfold(this) { // fは変化しないのでzにしなくてよい
      case Cons(h, t) if f(h()) => Some((h(), t())) // forの3番目
      case _ => None
    }
  }

  // 答え見た。
  // 謎。zipAllが3章に登場してなくない？
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some((Some(h()), Option.empty[B]) -> (t(), Empty))
      case (Empty, Cons(h, t)) => Some((Option.empty[A], Some(h())) -> (Empty -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())) -> (t1() -> t2()))
    }
  }

  def size(): Int = {
    foldRight(0)((_, t) => t + 1)
  }

  // EXERCISE 5.14
  def startsWith[A](s: Stream[A]): Boolean = {
    val size = s.size()
    if (this.size() < size) return false

    // ここでtoListする理由は、一見takeのあとにforAllがあることによって、takeの引数以降のストリームが処理されないように見えるけど、実際にはforAllで最後までストリームが処理される。
    // forAll内部ではCons(h, t)でパターンマッチしてるので、ストリームの長さが違う場合はfalseになる。
    // なのでtoListで一旦ストリームを正格化してしまい、takeの指定数に絞ってあげることで後続の比較を実施しないようにしている。
    Stream.zipWith_unfold(this, s)((a, b) => a == b).take(size).toList.forall(_ == true)
  }

  // EXERCISE 5.15
  def tails: Stream[Stream[A]] = {
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    }.append(Stream(Stream.empty))
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


  // EXERCISE 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val 無限a: Stream[A] = Stream.cons(a, 無限a)
    無限a
  }

  // EXERCISE 5.9
  def from(n: Int): Stream[Int] =
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

  // EXERCISE 5.12
  def fibs_unfold(): Stream[Int] = {
    def 現在状態Aと次の値Sを生成するf(現在状態: (Int, Int)): Option[(Int, (Int, Int))] = {
      現在状態 match { // 絶対マッチするのでは？
        case (n1, n2) => Some((n1, (n2, n1 + n2))) // 次の値タプル2つ目は「前回の結果と、次の値」になってるけど本当は次の値だけを返したい。初期状態Sと同じ型でないとだめなので仕方なく。
      }
    }

    unfold((0, 1))(現在状態Aと次の値Sを生成するf)
  }

  def from_undold(n: Int): Stream[Int] = {
    def 現在状態Aと次の値Sを生成するf(現在状態: Int): Option[(Int, Int)] = {
      Some(現在状態, 現在状態 + 1)
    }

    unfold(n)(現在状態Aと次の値Sを生成するf)
  }

  def constant_unfold[A](a: A): Stream[A] = {
    def 現在状態Aと次の値Sを生成するf(現在状態: A): Option[(A, A)] = {
      Some(現在状態, 現在状態)
    }

    unfold(a)(現在状態Aと次の値Sを生成するf)
  }

  def ones_unfold(): Stream[Int] = {
    def 現在状態Aと次の値Sを生成するf(現在状態: Int): Option[(Int, Int)] = {
      Some(現在状態, 現在状態)
    }

    unfold(1)(現在状態Aと次の値Sを生成するf)
  }

  // EXERCISE 5.13
  // 答え見た。みてもわからん。mapとシグネチャが違うのがそもそも想定外だった。
  // fに対して「現在状態Aと次の値Sを生成するf」と名前を付けてしまったのが考えを硬直させたように思う。現在状態は「fの引数」という名前にしたほうが良かった。
  // でもそう考えるとunfoldがなんのためにあるのかを理解できていない。あとfの戻り値がzも一緒に返すのもなぜなのかわかってない(head,tailの構造を前提としているから？)
  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    //    def 現在状態Aと次の値Sを生成するf(現在状態: A)(f: A => S): Option[(A, S)] = {
    //      Some((現在状態, f(現在状態)))
    //    }
    //
    //    def go(stream: Stream[A]): Stream[S] = {
    //      stream match {
    //        case Empty => empty
    //        case Cons(h, t) => cons(f(h()), go(t()))
    //      }
    //    }

    f(z).map((p: (A, S)) => cons(p._1, unfold(p._2)(f))).getOrElse(empty[A])
  }

  def zipWith_unfold[A](stream1: Stream[A], stream2: Stream[A])(f: (A, A) => A): Stream[A] = {
    unfold((stream1, stream2))({
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    })
  }
}
