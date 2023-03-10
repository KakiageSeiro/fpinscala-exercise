package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class リストを扱うその他の関数Test extends AnyFlatSpec with Diagrams with TimeLimits {
  "要素に1足す" should "よ" in {
    assert(リストを扱うその他の関数.addOne(List2(1, 2, 3)) == List2(2, 3, 4))
  }

  "doubleをStringに" should "変換" in {
    assert(リストを扱うその他の関数.doubleToString(List2(1.0, 2.0, 3.0)) == List2("1.0", "2.0", "3.0"))
  }

  "map" should "変換" in {
    assert(リストを扱うその他の関数.map(List2(1.0, 2.0, 3.0))((x:Double) => x.toString) == List2("1.0", "2.0", "3.0"))
    assert(リストを扱うその他の関数.map(List2(1.0, 2.0, 3.0))((x:Double) => x * x) == List2(1.0, 4.0, 9.0))
  }

  "filter" should "しぼる" in {
    assert(リストを扱うその他の関数.filter(List2(1, 2, 3, 4))(_ % 2 == 0) == List2(2, 4))
  }

  "flatMap" should "文脈付き変換" in {
    assert(リストを扱うその他の関数.flatMap(List2(1, 2, 3))(i => List2(i, i)) == List2(1, 1, 2, 2, 3, 3))
  }

  "flatMapをつかったfilter" should "しぼる" in {
    assert(リストを扱うその他の関数.flatMapFilter(List2(1, 2, 3, 4))(_ % 2 == 0) == List2(2, 4))
  }

  "2つのリストを" should "足す" in {
    assert(リストを扱うその他の関数.zip(List2(1, 2, 3), List2(4, 5, 6)) == List2(5, 7, 9))
  }

  "zipWith" should "よ" in {
    assert(リストを扱うその他の関数.zipWith(List2(1, 2, 3), List2(1, 2, 3))((a: Int, b: Int) => a + b) == List2(2, 4, 6))
    assert(リストを扱うその他の関数.zipWith(List2(1, 2, 3), List2(4, 5, 6))((a: Int, b: Int) => a + b) == List2(5, 7, 9))
    assert(リストを扱うその他の関数.zipWith(List2(1, 2, 3), List2(4, 5, 6))((a: Int, b: Int) => a * b) == List2(4, 10, 18))
  }

  "scanList" should "よ" in {
    assert(リストを扱うその他の関数.scanListを試す(List(1, 2, 3, 4)) == List(0, 1, 3, 6, 10))
  }

  "scanRight" should "よ" in {
    assert(リストを扱うその他の関数.scanRightを試す(List(1, 2, 3, 4)) == List(10, 9, 7, 4, 0))
  }
}
