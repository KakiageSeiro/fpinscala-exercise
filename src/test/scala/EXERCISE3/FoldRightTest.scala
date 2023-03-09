package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class FoldRightTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "かけざん" should "できる" in {
    assert(FoldRight.product2(List2(1, 2, 3, 4)) == 24)
  }
}
