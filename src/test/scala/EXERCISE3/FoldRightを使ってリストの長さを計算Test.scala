package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class FoldRightを使ってリストの長さを計算Test extends AnyFlatSpec with Diagrams with TimeLimits {
  "リストの長さ" should "とれる" in {
    assert(FoldRightを使ってリストの長さを計算.length(List2(1, 2, 3))== 3)
  }
}
