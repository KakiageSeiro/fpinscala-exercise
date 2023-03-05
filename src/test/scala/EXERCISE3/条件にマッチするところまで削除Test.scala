package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class 条件にマッチするところまで削除Test extends AnyFlatSpec with Diagrams with TimeLimits {
  "マッチするとき" should "消す" in {
    assert(条件にマッチするところまで削除.dropWhile(List2(1, 2, 3, 4), (a: Int) => a < 3) == List2(3, 4))
  }
}
