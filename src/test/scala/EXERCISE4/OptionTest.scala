package EXERCISE4

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class OptionTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "sequence" should "1つにまとめる" in {
    val list = List(Option.Some(1), Option.Some(2), Option.Some(3), Option.Some(4))
    assert(Option.sequence(list) == Option.Some(List(1, 2, 3, 4)))
  }

  "sequence" should "Noneが1つあったら結果もNone" in {
    val list = List(Option.Some(1), Option.None, Option.Some(3), Option.Some(4))
    assert(Option.sequence(list) == Option.None)
  }
}
