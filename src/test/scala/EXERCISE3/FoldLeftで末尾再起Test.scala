package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class FoldLeftで末尾再起Test extends AnyFlatSpec with Diagrams with TimeLimits {
  "うごかすだけ" should "だよ" in {
    assert(FoldLeftで末尾再起.foldLeft(List2(1, 2, 3), 0)(_ + _) == 6)
  }

  "sum" should "だよ" in {
    assert(FoldLeftで末尾再起.sum(List2(1, 2, 3)) == 6)
  }

  "product" should "だよ" in {
    assert(FoldLeftで末尾再起.product(List2(1, 2, 3, 4)) == 24)
  }

  "length" should "だよ" in {
    assert(FoldLeftで末尾再起.length(List2(1, 2, 3, 4)) == 4)
  }

  "reverse" should "だよ" in {
    assert(FoldLeftで末尾再起.reverse(List2(1, 2, 3, 4)) == Cons(4, Cons(3, Cons(2, Cons(1, Nil)))))
  }

  "append" should "だよ" in {
    assert(FoldLeftで末尾再起.append(List2(1, 2, 3), List2(4, 5, 6)) == List2(1, 2, 3, 4, 5, 6))
    assert(FoldLeftで末尾再起.append2(List2(1, 2, 3), List2(4, 5, 6)) == List2(1, 2, 3, 4, 5, 6))
  }

  "flatten" should "だよ" in {
    assert(FoldLeftで末尾再起.flatten(List2(List2(1, 2, 3), List2(4, 5, 6))) == List2(1, 2, 3, 4, 5, 6))
  }
}
