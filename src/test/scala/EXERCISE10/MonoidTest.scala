package EXERCISE10

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}
import scala.concurrent.TimeoutException


class MonoidTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "bagMonoidをつかったgroupByからのcountみたいなやつ" should "カウントができる" in {
    val inputSeq: IndexedSeq[String] = IndexedSeq("one", "つー", "さん", "one", "つー", "さん", "one", "つー", "さん", "4")

    val result: Map[String, Int] = bagMonoidObject.bag(inputSeq)
    assert(result("one") == 3)
    assert(result("つー") == 3)
    assert(result("さん") == 3)
    assert(result("4") == 1)
  }
}