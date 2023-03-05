package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class 色々なパターンマッチTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "色々なパターンマッチ" should "3番目のやつになるよ" in {
    assert(色々なパターンマッチ.どれになる() == 3)
  }
}
