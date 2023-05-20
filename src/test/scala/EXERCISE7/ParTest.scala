package EXERCISE7

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec


class ParTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "map2" should "ペア" in {
    val result = Par.map2(Par.unit(10), Par.unit(20))(_ + _)
    assert(result == Par.unit(30))
  }

  "sum" should "たす" in {
    val result = Par.sum(IndexedSeq(1, 2, 3, 4, 5))
    assert(result == Par.unit(15))
  }
}










