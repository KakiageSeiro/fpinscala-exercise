import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class フィボナッチ数Test extends AnyFlatSpec with Diagrams with TimeLimits {
  "n番目のフィボナッチ数" should "取れる" in {
    assert(フィボナッチ数.fib(1) == 0)
    assert(フィボナッチ数.fib(2) == 1)
    assert(フィボナッチ数.fib(3) == 1)
    assert(フィボナッチ数.fib(4) == 2)
    assert(フィボナッチ数.fib(5) == 3)
    assert(フィボナッチ数.fib(6) == 5)
    assert(フィボナッチ数.fib(7) == 8)
    assert(フィボナッチ数.fib(8) == 13)
    assert(フィボナッチ数.fib(9) == 21)
  }

  "ループカウント無しでもn番目のフィボナッチ数" should "取れる" in {
    assert(フィボナッチ数.fib2(1) == 0)
    assert(フィボナッチ数.fib2(2) == 1)
    assert(フィボナッチ数.fib2(3) == 1)
    assert(フィボナッチ数.fib2(4) == 2)
    assert(フィボナッチ数.fib2(5) == 3)
    assert(フィボナッチ数.fib2(6) == 5)
    assert(フィボナッチ数.fib2(7) == 8)
    assert(フィボナッチ数.fib2(8) == 13)
    assert(フィボナッチ数.fib2(9) == 21)
  }

}
