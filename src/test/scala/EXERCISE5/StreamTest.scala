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


  "forAll" should "すべて該当" in {
    val cons = Stream.cons({println("Consの要素評価1"); 1 }, Stream.cons({println("Consの要素評価2"); 2 }, Stream.cons({println("Consの要素評価3"); 3 }, Stream.empty)))
    assert(cons.forAll(_ < 4))
  }

  it should "先頭2つまで該当" in {
    val cons = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(throw new Exception("tail evaluated"), Stream.empty))))
    assert(cons.forAll(_ < 3))
  }


  "takeWhile_foldRight" should "takeWhileのテストと同じ" in {
    val s = Stream(1, 2, 3, 4, 5)
    val result = s.takeWhile_foldRight(_ < 3).toList
    assert(result == List(1, 2))

    val s2 = Stream(1, 2, 3, 4, 5)
    val result2 = s2.takeWhile_foldRight(_ % 2 == 1).toList
    assert(result2 == List(1))

    val s3 = Stream.empty[Int]
    val result3 = s3.takeWhile_foldRight(_ < 3).toList
    assert(result3 == List.empty[Int])

    val s4 = Stream(1, 2, 3, 4, 5)
    val result4 = s4.takeWhile_foldRight(_ > 5).toList
    assert(result4 == List.empty[Int])
  }

  "headOption_foldRight" should "とれる" in {
    val cons = Cons(() => 1, () => Empty)
    assert(cons.headOption_foldRight == Some(1))
  }

  "headOption_foldRight" should "とれる2" in {
    val cons = Stream.cons(
      {
        println("Consの要素評価1"); () => 1
      },
      Stream.cons(
        {
          println("Consの要素評価2"); () => 2
        },
        Stream.cons(
          {
            println("Consの要素評価3"); () => 3
          },
          Stream.empty)))

    val result: Option[() => Int] = cons.headOption_foldRight
    result match {
      case Some(x) => assert(x() == 1)
      case None => assert(false)
    }
  }

  "headOption_foldRight" should "とれる3" in {
    val cons = Stream.cons(
      {
        println("Consの要素評価1"); 1
      },
      Stream.cons(
        {
          println("Consの要素評価2"); 2
        },
        Stream.cons(
          {
            println("Consの要素評価3"); 3
          },
          Stream.empty)))

    val result: Option[Int] = cons.headOption_foldRight
    result match {
      case Some(x) => assert(x == 1)
      case None => assert(false)
    }
  }

  "map" should "変換できる" in {
    val cons = Stream.cons(
      {
        println("Consの要素評価1");
        1
      },
      Stream.cons(
        {
          println("Consの要素評価2");
          2
        },
        Stream.cons(
          {
            println("Consの要素評価3");
            3
          },
          Stream.empty)))

    val result = cons.map(_ + 1).toList
    assert(result == List(2, 3, 4))
  }

  "filter" should "しぼれる" in {
    val cons = Stream.cons(
      {
        println("Consの要素評価1");
        1
      },
      Stream.cons(
        {
          println("Consの要素評価2");
          2
        },
        Stream.cons(
          {
            println("Consの要素評価3");
            3
          },
          Stream.empty)))

    val result = cons.filter(_ % 2 == 0).toList
    assert(result == List(2))
  }

  "append" should "末尾に追加できる" in {
    val cons1 = Stream.cons({println("Consの要素評価1"); 1}, Stream.cons({println("Consの要素評価2"); 2}, Stream.cons({println("Consの要素評価3"); 3}, Stream.empty)))
    val cons2 = Stream.cons({println("Consの要素評価4"); 4}, Stream.cons({println("Consの要素評価5"); 5}, Stream.cons({println("Consの要素評価6"); 6}, Stream.empty)))

    val result = cons1.append(cons2).toList
    assert(result == List(1, 2, 3, 4, 5, 6))
  }

  "append" should "スーパータイプを追加" in {
    val cons = Stream.cons({println("Consの要素評価1"); 1}, Stream.cons({println("Consの要素評価2"); 2}, Stream.cons({println("Consの要素評価3"); 3}, Stream.empty)))
    val stream = Stream(1, 2, 3)

    val result = cons.append(stream).toList
    assert(result == List(1, 2, 3, 1, 2, 3))
  }

  "append" should "サブタイプを追加" in {
    val cons = Stream.cons({println("Consの要素評価1"); 1}, Stream.cons({println("Consの要素評価2"); 2}, Stream.cons({println("Consの要素評価3"); 3}, Stream.empty)))
    val stream = Stream(1, 2, 3)

    val result = stream.append(cons).toList
    assert(result == List(1, 2, 3, 1, 2, 3))
  }

  "append" should "スーパータイプ同士" in {
    val stream1 = Stream(1, 2, 3)
    val stream2 = Stream(1, 2, 3)

    val result = stream1.append(stream2).toList
    assert(result == List(1, 2, 3, 1, 2, 3))
  }

  "flatMap" should "文脈付き変換できる" in {
    val cons = Stream.cons({println("Consの要素評価1"); 1}, Stream.cons({println("Consの要素評価2"); 2}, Stream.cons({println("Consの要素評価3"); 3}, Stream.empty)))

    val result = cons.flatMap(x => Stream(x + 1)).toList
    assert(result == List(2, 3, 4))
  }

  "constant" should "無限" in {
    val 無限a = Stream.constant(42)

    val result1 = 無限a.take(3).toList
    assert(result1 == List(42, 42, 42))

    val result2 = 無限a.take(5).toList
    assert(result2 == List(42, 42, 42, 42, 42))
  }

  "from" should "無限" in {
    val nからの無限 = Stream.from(3)

    val result1 = nからの無限.take(3).toList
    assert(result1 == List(3, 4, 5))

    val result2 = nからの無限.take(5).toList
    assert(result2 == List(3, 4, 5, 6, 7))
  }

  "fibs" should "無限" in {
    val fibs = Stream.fibs()

    val result1 = fibs.take(3).toList
    assert(result1 == List(0, 1, 1))

    val result2 = fibs.take(10).toList
    assert(result2 == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  "unfold" should "無限" in {
    def generateNext(value: Int): Option[(Int, Int)] = {
      if (value < 5)
        Some((value, value + 1))
      else
        None
    }

    val result = Stream.unfold(0)(generateNext).toList
    val expected = List(0, 1, 2, 3, 4)
    assert(result == expected)
  }

  "fibs_unfold" should "無限" in {
    val result = Stream.fibs_unfold().take(10).toList
    val expected = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    assert(result == expected)
  }

  "from_unfold" should "無限" in {
    val nからの無限 = Stream.from_undold(3)

    val result1 = nからの無限.take(3).toList
    assert(result1 == List(3, 4, 5))

    val result2 = nからの無限.take(5).toList
    assert(result2 == List(3, 4, 5, 6, 7))
  }

  "constant_unfold" should "無限" in {
    val 無限a = Stream.constant_unfold(42)

    val result1 = 無限a.take(3).toList
    assert(result1 == List(42, 42, 42))

    val result2 = 無限a.take(5).toList
    assert(result2 == List(42, 42, 42, 42, 42))
  }

  "ones_unfold" should "無限" in {
    val 無限1 = Stream.ones_unfold()

    val result1 = 無限1.take(3).toList
    assert(result1 == List(1, 1, 1))

    val result2 = 無限1.take(5).toList
    assert(result2 == List(1, 1, 1, 1, 1))
  }

  "unfoldViaMap" should "無限" in {
    val stream = Stream.unfoldViaMap(1)(s => Some((s, s + 1))).take(5).toList
    val expected = Stream(1, 2, 3, 4, 5).toList
    assert(stream == expected)
  }

  "take_unfold" should "先頭から何個かとる" in {
    val stream = Stream.cons({
      println("Consの要素評価1"); 1
    }, Stream.cons({
      println("Consの要素評価2"); 2
    }, Stream.cons({
      println("Consの要素評価3"); 3
    }, Stream.empty)))

    val result = stream.take_unfold(2).toList
    assert(result == List(1, 2))
  }

  "takeWhile_unfold" should "とれる〜" in {
    val s = Stream(1, 2, 3, 4, 5)
    val result = s.takeWhile_unfold(_ < 3).toList
    assert(result == List(1, 2))

    val s2 = Stream(1, 2, 3, 4, 5)
    val result2 = s2.takeWhile_unfold(_ % 2 == 1).toList
    assert(result2 == List(1))

    val s3 = Stream.empty[Int]
    val result3 = s3.takeWhile_unfold(_ < 3).toList
    assert(result3 == List.empty[Int])

    val s4 = Stream(1, 2, 3, 4, 5)
    val result4 = s4.takeWhile_unfold(_ > 5).toList
    assert(result4 == List.empty[Int])
  }

  "zipWith" should "よ" in {
    assert(Stream.zipWith_unfold(Stream(1, 2, 3), Stream(1, 2, 3))((a: Int, b: Int) => a + b).toList == Stream(2, 4, 6).toList)
    assert(Stream.zipWith_unfold(Stream(1, 2, 3), Stream(4, 5, 6))((a: Int, b: Int) => a + b).toList == Stream(5, 7, 9).toList)
    assert(Stream.zipWith_unfold(Stream(1, 2, 3), Stream(4, 5, 6))((a: Int, b: Int) => a * b).toList == Stream(4, 10, 18).toList)
  }

  "size" should "サイズ" in {
    assert(Stream(1, 2, 3).size == 3)
    assert(Stream(1, 2, 3, 4, 5).size == 5)
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).size == 10)
  }

  "startsWith" should "先頭から始まる" in {
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2, 3)))
    assert(!Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)))
    assert(!Stream(1, 2, 3).startsWith(Stream(1, 3)))
    assert(!Stream(1, 2, 3).startsWith(Stream(2, 3)))
    assert(!Stream(1, 2, 3).startsWith(Stream(2, 3, 4)))
  }

  "tails" should "サフィックス" in {
    assert(Stream(1, 2, 3).tails.map(_.toList).toList == List(List(1, 2, 3), List(2, 3), List(3), List.empty[Int]))
  }
}
