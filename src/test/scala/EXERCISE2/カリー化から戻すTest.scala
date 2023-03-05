package EXERCISE2

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class カリー化から戻すTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "カリー化された関数を" should "AとBからCを生成する関数にする" in {
    val f = (a: Char, b: Int) => a.toString + b.toString
    val fと同じシグネチャの関数 = カリー化から戻す.uncurry(カリー化.curry(f))
    assert(fと同じシグネチャの関数('a', 1) == "a1")
  }
}
