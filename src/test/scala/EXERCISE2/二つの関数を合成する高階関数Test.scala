package EXERCISE2

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class 二つの関数を合成する高階関数Test extends AnyFlatSpec with Diagrams with TimeLimits {
  "二つの関数を合成する高階関数" should "AからCの関数が返る" in {
    val IntからStringにする関数 = (b: Int) => b.toString
    val StringからCharにする関数 = (a: String) => a.charAt(0)
    val IntからCharにする関数 = 二つの関数を合成する高階関数.compose(StringからCharにする関数, IntからStringにする関数)
    assert(IntからCharにする関数(1) == '1')
  }
}
