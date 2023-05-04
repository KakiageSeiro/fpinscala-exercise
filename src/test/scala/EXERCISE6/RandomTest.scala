package EXERCISE6

import EXERCISE6.Random.{Rand, flatMap, int, map2, map2_flatMap, map_flatMap, sequence}
import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec


class RandomTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "SimpleRNG" should "なんかいやっても同じ" in {
    val int_rng: (Int, RNG) = SimpleRNG(1).nextInt
    assert(int_rng._1 == 384748)
    assert(int_rng._2 == SimpleRNG(25214903928L))
  }

  "nonNegativeInt" should "マイナスは返らない" in {
    val result1: (Int, RNG) = Random.nonNegativeInt(SimpleRNG(1))
    assert(result1._1 == 384748)
    assert(result1._2 == SimpleRNG(25214903928L))

    val result2: (Int, RNG) = Random.nonNegativeInt(SimpleRNG(-1)) // これで-384749が生成される
    assert(result2._1 == 384748) // -384749 がプラスになり384749。-1して384748になる。
    assert(result2._2 == SimpleRNG(281449761806750L))

    val result3: (Int, RNG) = Random.nonNegativeInt(SimpleRNG(0))
    assert(result3._1 == 0)
    assert(result3._2 == SimpleRNG(11))
  }

  "double" should "小数" in {
    val result1: (Double, RNG) = Random.double(SimpleRNG(1))
    assert(result1._1 == 1.7916224896907806E-4)
    assert(result1._2 == SimpleRNG(25214903928L)) // RNGは同じものが返ってくるよ

    val result2: (Double, RNG) = Random.double(SimpleRNG(-1))
    assert(result2._1 == 1.7916224896907806E-4)
    assert(result2._2 == SimpleRNG(281449761806750L))

    val result3: (Double, RNG) = Random.double(SimpleRNG(0))
    assert(result3._1 == 0)
    assert(result3._2 == SimpleRNG(11))
  }

  "intDouble" should "ペア" in {
    val result1: ((Int, Double), RNG) = Random.intDouble(SimpleRNG(1))
    assert(result1._1._1 == 384748)
    assert(result1._1._2 == 0.5360936457291245)
    assert(result1._2 == SimpleRNG(206026503483683L))
  }

  "doubleInt" should "ペア" in {
    val result1: ((Double, Int), RNG) = Random.doubleInt(SimpleRNG(1))
    assert(result1._1._1 == 1.7916224896907806E-4)
    assert(result1._1._2 == -1151252339)
    assert(result1._2 == SimpleRNG(206026503483683L))
  }

  "doubledoubledouble" should "ペア" in {
    val result1: ((Double, Double, Double), RNG) = Random.double3(SimpleRNG(1))
    assert(result1._1._1 == 1.7916224896907806E-4)
    assert(result1._1._2 == 0.5360936457291245)
    assert(result1._1._3 == 0.2558267889544368)
    assert(result1._2 == SimpleRNG(245470556921330L))
  }

  "ints" should "ペア" in {
    val result1: (List[Int], RNG) = Random.ints(3)(SimpleRNG(1))
    assert(result1._1 == List(-549383847, -1151252339, 384748))
    assert(result1._2 == SimpleRNG(245470556921330L))
  }

  "double_map" should "小数" in {
    val result1: (Double, RNG) = Random.double_map(SimpleRNG(1))
    assert(result1._1 == 1.7916224896907806E-4)
    assert(result1._2 == SimpleRNG(25214903928L)) // RNGは同じものが返ってくるよ

    val result2: (Double, RNG) = Random.double_map(SimpleRNG(-1))
    assert(result2._1 == 1.7916224896907806E-4)
    assert(result2._2 == SimpleRNG(281449761806750L))

    val result3: (Double, RNG) = Random.double_map(SimpleRNG(0))
    assert(result3._1 == 0)
    assert(result3._2 == SimpleRNG(11))
  }

  "map2" should "ペア" in {
    val ra: Rand[Int] = rng => (10, rng)
    val rb: Rand[String] = rng => ("test", rng)

    val result: (String, RNG) = map2(ra, rb)((a, b) => a.toString + b)(SimpleRNG(1))
    assert(result._1 == "10test")
    assert(result._2 == SimpleRNG(1))
  }

  "sequence" should "しーくぅえんす" in {
    val r1: Rand[Int] = rng => (10, rng)
    val r2: Rand[Int] = rng => (10, rng)
    val r3: Rand[Int] = rng => (10, rng)
    val list: List[Rand[Int]] = r1 :: r2 :: r3 :: Nil

    val result: Rand[List[Int]] = sequence(list)
    assert(result(SimpleRNG(1))._1 == List(10, 10, 10))
  }

  "ints_sequence" should "ペア" in {
    val result1: (List[Int], RNG) = Random.ints_sequence(3)(SimpleRNG(1))
    assert(result1._1 == List(-549383847, -1151252339, 384748))
    assert(result1._2 == SimpleRNG(245470556921330L))
  }

  "nonNegativeLessThen" should "nの範囲内でマイナスじゃない" in {
    val result = Random.nonNegativeLessThen(10)(SimpleRNG(1))
    assert(result._1 == 8)
    assert(result._2 == SimpleRNG(25214903928L))
  }

  "flatMap" should "最初のRandの結果に与えられた関数を適用し2番目のRandの結果を返す" in {
    def constant[A](a: A): Rand[A] = rng => (a, rng)
    val rng = SimpleRNG(42)
    val composedRand: Rand[String] = flatMap(int)(i => constant(i.toString))

    val (result, _) = composedRand(rng)
    assert(result == "16159453")
  }

  "nonNegativeLessThen_flatMap" should "nの範囲内でマイナスじゃない" in {
    val result = Random.nonNegativeLessThen_flatMap(10)(SimpleRNG(1))
    assert(result._1 == 8)
    assert(result._2 == SimpleRNG(25214903928L))
  }

  "map_flatMap" should "ペア" in {
    val r: Rand[Int] = rng => (10, rng)

    val result: (String, RNG) = map_flatMap(r)(a => a.toString)(SimpleRNG(1))
    assert(result._1 == "10")
    assert(result._2 == SimpleRNG(1))
  }

  "map2_flatMap" should "ペア" in {
    val ra: Rand[Int] = rng => (10, rng)
    val rb: Rand[String] = rng => ("test", rng)

    val result: (String, RNG) = map2_flatMap(ra, rb)((a, b) => a.toString + b)(SimpleRNG(1))
    assert(result._1 == "10test")
    assert(result._2 == SimpleRNG(1))
  }
}










