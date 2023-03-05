package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class 先頭以外の要素を取るTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "tail" should "後ろ取る" in {
    assert(先頭以外の要素を取る.tail(List2(1, 2, 3)) == List2(2, 3))
  }
}
