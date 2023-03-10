package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class サブシーケンスが含まれているか調べるTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "サブシーケンスが" should "含まれているか" in {
    assert(サブシーケンスが含まれているか調べる.hasSubsequence(List2(1, 2, 3, 4), List2(1, 2)))
    assert(サブシーケンスが含まれているか調べる.hasSubsequence(List2(1, 2, 3, 4), List2(2, 3)))
    assert(サブシーケンスが含まれているか調べる.hasSubsequence(List2(1, 2, 3, 4), List2(4)))
  }

}
