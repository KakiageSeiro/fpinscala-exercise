package EXERCISE2

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class カリー化Test extends AnyFlatSpec with Diagrams with TimeLimits {
  "AとBからCを生成する関数をカリー化" should "別々に渡せるようにする" in {
    def CharとIntからStringを生成する関数(a: Char, b: Int): String = a.toString + b.toString
    val f: (Char, Int) => String = CharとIntからStringを生成する関数

    assert(カリー化.curry(f)('a')(1) == "a1")
  }
}
