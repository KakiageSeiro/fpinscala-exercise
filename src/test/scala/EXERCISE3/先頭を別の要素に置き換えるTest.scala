package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class 先頭を別の要素に置き換えるTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "setHead" should "置き換えれる" in {
    assert(先頭を別の要素に置き換える.setHead(List2(1, 2, 3), 4) == List2(4, 2, 3))
  }
}
