package EXERCISE2

import EXERCISE2.フィボナッチ数
import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class ソートされてるかを調べるTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "ソートされてると" should "true" in
    assert(ソートされてるかを調べる.isSorted(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), (x: Int, y: Int) => x < y))
    assert(!ソートされてるかを調べる.isSorted(Array(3, 2, 3, 4, 5, 6, 7, 8, 9, 10), (x: Int, y: Int) => x < y))

  "文字列でもソートされてると" should "true" in
    assert(ソートされてるかを調べる.isSorted(Array("abc","acc","bbb"), (x: String, y: String) => x < y))
    assert(!ソートされてるかを調べる.isSorted(Array("ab","aa","bbb"), (x: String, y: String) => x < y))

}
