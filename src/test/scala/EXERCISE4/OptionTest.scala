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

  def parseNumber(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _: NumberFormatException => None
    }
  }

  // なぜかtraverse呼出し部分でparseNumberが型合ってないと言われる
//  "traverse" should "1つにまとめて変換" in {
//    val list = List("1", "2", "3", "4")
//    assert(Option.traverse(list)(_.toIntOption) == Option.Some(List(1, 2, 3, 4)))
//  }
//  "traverse" should "Noneが1つあったら結果もNone" in {
//    val list = List("1", None, "3", "4")
//    assert(Option.traverse(list)(parseNumber) == Option.None)
//  }
}
