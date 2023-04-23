package EXERCISE5

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class StreamTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "headOption" should "とれる" in {
    val cons = Cons(() => 1, () => Empty)
    assert(cons.headOption == Some(1))
  }

  "headOption" should "とれる2" in {
    // 最後のConsの第二引数としてStream.emptyが渡されるが、Stream[Nothing]として渡されるので型が合わない
//    val cons1 = Cons(
//      { println("Consの要素評価1"); () => 1},
//      Cons(
//        { println("Consの要素評価2"); () => 2},
//        Cons(
//          { println("Consの要素評価3"); () => 3},
//          Stream.empty)))

    // () => Emptyを渡すと、Cons[Int]になる。型が合わないのもよく分からないが、Cons[Int]になるのもよく分からない。
//    val cons2 = Cons(
//      { println("Consの要素評価1"); () => 1},
//      Cons(
//        { println("Consの要素評価2"); () => 2},
//        Cons(
//          { println("Consの要素評価3"); () => 3},
//          () => Empty)))

    // これなら型が合う。確かに Stream.empty は Stream[A] を返す関数だけど
    // それなら cons1,2 も上手くいくはずでは？
    // tl: => Stream[A]とtl: () => Stream[A]の違い
    val cons = Stream.cons(
      { println("Consの要素評価1"); () => 1 },
      Stream.cons(
        { println("Consの要素評価2"); () => 2 },
        Stream.cons(
          { println("Consの要素評価3"); () => 3 },
          Stream.empty)))

    val result: Option[() => Int] = cons.headOption
    result match {
      case Some(x) => assert(x() == 1)
      case None => assert(false)
    }
  }

  "headOption" should "とれる3" in {
    // Stream.consに渡すのは、() => Aの関数だと思ってたので{}にprintと() => Aを2つ書いていた。
    // {}にした時点で、() => Aの関数になっているので、最後に評価されるAであるIntの1とかを書けばよかった。
    val cons = Stream.cons(
      { println("Consの要素評価1"); 1 },
      Stream.cons(
        { println("Consの要素評価2"); 2 },
        Stream.cons(
          { println("Consの要素評価3"); 3 },
          Stream.empty)))

    val result: Option[Int] = cons.headOption
    result match {
      case Some(x) => assert(x == 1)
      case None => assert(false)
    }
  }

//  "toList" should "Listになる" in {
//    // {}に() => Aを入れちゃったパターンでは() => Aのリストができるよ
//    // Stream.cons関数にとっての型パラメータが() => Aになるからね！
//    val cons = Stream.cons(
//      { println("Consの要素評価1"); () => 1 },
//      Stream.cons(
//        { println("Consの要素評価2"); () => 2 },
//        Stream.cons(
//          { println("Consの要素評価3"); () => 3 },
//          Stream.empty)))
//
//    val list: List[() => Int] = cons.toList
//    assert(list == List(() => 1, () => 2, () => 3))
//  }

  "toList" should "StreamからList" in {
    val stream = Stream(1, 2, 3)
    val list = stream.toList
    assert(list == List(1, 2, 3))
  }

  "toList" should "ConsからList" in {
    val stream = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))
    val list = stream.toList
    assert(list == List(1, 2, 3))
  }

  "toList" should "ConsからList_ログ入り" in {
    val stream = Stream.cons({println("Consの要素評価1"); 1 }, Stream.cons({println("Consの要素評価2"); 2 }, Stream.cons({println("Consの要素評価3"); 3 }, Stream.empty)))
    val list = stream.toList
    assert(list == List(1, 2, 3))
  }


  "take" should "先頭から何個かとる" in {
    val stream = Stream.cons({ println("Consの要素評価1"); 1 }, Stream.cons({ println("Consの要素評価2"); 2 }, Stream.cons({ println("Consの要素評価3"); 3 }, Stream.empty)))

    val result = stream.take(2).toList
    assert(result == List(1, 2))
  }

  // 以下テストケースはChatGPTに考えてもらった。
  // 最後のテストはthrowが混ざってることで非正格であることが分かりやすいし、自分ではパッと思いつかなかったのでありがたい。
  "take" should "return a new Stream with the first n elements of the original Stream" in {
    val stream = Stream(1, 2, 3, 4, 5)
    val result = stream.take(3).toList
    assert(result == List(1, 2, 3))
  }

  it should "return an empty Stream if n is zero" in {
    val stream = Stream(1, 2, 3, 4, 5)
    val result = stream.take(0).toList
    assert(result == List())
  }

  it should "return the entire Stream if n is larger than the length of the Stream" in {
    val stream = Stream(1, 2, 3, 4, 5)
    val result = stream.take(10).toList
    assert(result == List(1, 2, 3, 4, 5))
  }

  it should "not evaluate the tail of the original Stream beyond the necessary elements" in {
    val stream = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(throw new Exception("tail evaluated"), Stream.empty))))
    val result = stream.take(2).toList
    assert(result == List(1, 2))
  }

  // またChatGPT様作のテスト。ありがたや〜
  "drop" should "drop first n elements and return the rest" in {
    val s = Stream(1, 2, 3, 4, 5)
    assert(s.drop(0).toList == List(1, 2, 3, 4, 5))
    assert(s.drop(2).toList == List(3, 4, 5))
    assert(s.drop(5).toList == List())
    assert(s.drop(6).toList == List())
  }

  "takeWhile" should "これもChatGPT様作のテストだよ〜" in {
    val s = Stream(1, 2, 3, 4, 5)
    val result = s.takeWhile(_ < 3).toList
    assert(result == List(1, 2))

    val s2 = Stream(1, 2, 3, 4, 5)
    val result2 = s2.takeWhile(_ % 2 == 1).toList
    assert(result2 == List(1))

    val s3 = Stream.empty[Int]
    val result3 = s3.takeWhile(_ < 3).toList
    assert(result3 == List.empty[Int])

    val s4 = Stream(1, 2, 3, 4, 5)
    val result4 = s4.takeWhile(_ > 5).toList
    assert(result4 == List.empty[Int])
  }

  "takeとtoList" should "いっしょにつかうよ" in {
    val list: List[Int] = Stream(1, 2, 3).take(2).toList
    assert(list == List(1, 2))
  }
}
