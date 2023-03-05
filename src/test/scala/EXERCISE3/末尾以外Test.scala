package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class 末尾以外Test extends AnyFlatSpec with Diagrams with TimeLimits {
  "末尾以外に" should "なったよ" in {
    assert(末尾以外.init(List2(1, 2, 3, 4)) == List2(1, 2, 3))
  }
}
