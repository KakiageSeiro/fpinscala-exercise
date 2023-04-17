package EXERCISE4

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class EitherTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "map" should "変換" in {
    val f = (a :Int) => a + 1

    Right(1).map(f) == Right(2)
    Left("error").map(f) == Left("error")
  }

  "flatMap" should "文脈付き変換" in {
    val f = (a: Int) => Right(a + 1)

    Right(1).flatMap(f) == Right(2)
    Left("error").flatMap(f) == Left("error")
  }

  "orElse" should "ないときは指定した値" in {
    val b = Right(2)

    Right(1).oeElse(b) == Right(2)
    Left("error").oeElse(b) == Left("error")
  }

  "map2" should "二つの値から何かつくる" in {
    val f = (a: Int, b:Int) => a.toString + b.toString
    val b = Right(2)

    Right(1).map2(b)(f) == Right("12")
    Left("error").map2(b)(f) == Left("error")
  }

  "sequence" should "Either2が要素のListをただのListにする_失敗するかもなのでEither2でラップして返却される" in {
    val list = List(Right(1), Right(2), Right(3), Right(4))

    Either2.sequence(list) == Right(List(1, 2, 3, 4))
  }

  "sequence" should "Leftが混じってるとダメ" in {
    val list1 = List(Left("err1"), Right(2), Right(3), Right(4))
    val list2 = List(Right(1), Right(2), Right(3), Left("err2"))

    Either2.sequence(list1) == Left("err1")
    Either2.sequence(list2) == Left("err2")
  }

  "traverse" should "AのListをBのListにして文脈Gでくるんで返す" in {
    val list = List("1", "2", "3", "4")
    val f = (s: String) => Right(s.toInt)

    Either2.traverse(list)(f) == Right(List(1, 2, 3, 4))
  }

  "traverse_for" should "AのListをBのListにして文脈Gでくるんで返す" in {
    val list = List("1", "2", "3", "4")
    val f = (s: String) => Right(s.toInt)

    Either2.traverse_for(list)(f) == Right(List(1, 2, 3, 4))
  }

}
