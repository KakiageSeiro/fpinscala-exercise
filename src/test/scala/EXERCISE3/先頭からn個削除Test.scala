package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class 先頭からn個削除Test extends AnyFlatSpec with Diagrams with TimeLimits {
  "n個" should "消す" in {
    assert(先頭からn個削除.drop(List2(1, 2, 3), 2) == List2(3))
  }
}
