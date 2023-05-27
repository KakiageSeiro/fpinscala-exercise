package EXERCISE7

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}
import scala.concurrent.TimeoutException


class ParTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "map2" should "ペア" in {
    val result: Par.Par[Int] = Par.map2(Par.unit(10), Par.unit(20))(_ + _)
    assert(result == Par.unit(30))
  }

  "sum" should "たす" in {
    val result = Par.sum(IndexedSeq(1, 2, 3, 4, 5))
    assert(result == Par.unit(15))
  }

  "map2Timeouts" should "たす" in {
    // newFixedThreadPoolはスレッド数を指定してExecutorServiceを作成する
    val es: ExecutorService = Executors.newFixedThreadPool(2)

    // テスト用の関数
    def sum(a: Int, b: Int): Int = a + b

    // ここがPar.Parになるのやだな。objectではなくcase classに全部定義してしまえばよかったけどめんどくさいからこのままで
    val parA: Par.Par[Int] = Par.unit {
      Thread.sleep(1000) // 1秒待機
      1
    }
    val parB: Par.Par[Int] = Par.unit(2)
    val parC: ExecutorService => Future[Int] = Par.map2Timeouts(parA, parB)(sum)
    val result = Par.run(es)(parC)
    assert(result.get() == 3)

    es.shutdown()
  }

  "map2Timeouts" should "タイムアウトしない" in {
    val es: ExecutorService = Executors.newFixedThreadPool(2)

    // テスト用の関数
    def sum(a: Int, b: Int): Int = a + b

    val parA: Par.Par[Int] = Par.unit {
      Thread.sleep(2000) // 2秒待機
      1
    }
    val parB: Par.Par[Int] = Par.unit {
      Thread.sleep(1000) // 1秒待機
      2
    }
    val parC = Par.map2Timeouts(parA, parB)(sum)
    val result = Par.run(es)(parC)

    // タイムアウト時間を指定しないのでタイムアウトにならない
    assert(result.get() == 3)

    es.shutdown()
  }

  "map2Timeouts" should "タイムアウトする" in {
    val es: ExecutorService = Executors.newFixedThreadPool(2)

    // テスト用の関数
    def sum(a: Int, b: Int): Int = a + b

    val parA: Par.Par[Int] = Par.unit {
      Thread.sleep(2000) // 2秒待機
      1
    }
    val parB: Par.Par[Int] = Par.unit {
      Thread.sleep(1000) // 1秒待機
      2
    }
    val parC = Par.map2Timeouts(parA, parB)(sum)
    val result = Par.run(es)(parC)

    // タイムアウトエラーがスローされるはずだがされない。なんでだ
    intercept[TimeoutException] {
      result.get(1, TimeUnit.NANOSECONDS)
    }

    es.shutdown()
  }
}










