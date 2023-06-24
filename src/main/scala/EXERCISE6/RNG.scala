package EXERCISE6

import scala.annotation.tailrec

// ランダム数生成器
trait RNG {
  def nextInt: (Int, RNG)
}

// 参照透過な生成器。テストしやすい
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed: Long = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG: SimpleRNG = SimpleRNG(newSeed)
    val n: Int = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  // EXERCISE 6.1
  // 生成器から○○つくる(これはマイナスではない数字)
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    val マイナスではない整数 =
      if (i < 0) -i - 1 // マイナスだったらプラスにする。0始まりなので-1する
      else i
    (マイナスではない整数, r)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (i, rng2) => (i % 2 == 0, rng2)
    }

  // EXERCISE 6.2
  // 生成器から○○つくる(これは小数)
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val double = i / (Int.MaxValue.toDouble + 1) // intの最大値より大きい値で割れば、必ず0.nになる
    (double, r)
  }

  // EXERCISE 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // EXERCISE 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, rng: RNG, list: List[Int]): (List[Int], RNG) = {
      if (count == 0) return (list, rng)

      val (i, r) = rng.nextInt
      loop(count - 1, r, i :: list)
    }

    loop(count, rng, List())
  }

  // RNGをとって、結果と一緒に返すやつのエイリアス
  // 生成期から「結果と"次の結果生成器"のセット」を返す
  type Rand[+A] = RNG => (A, RNG)
  // こうやって使う
  val int: Rand[Int] = _.nextInt

  // EXERCISE 6.5
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, r) = s(rng)
      (f(a), r)
    }
  }

  def double_map(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))(rng)
  }

  // EXERCISE 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }
  }

  // 2つの生成器を受け取って、2つの結果を返す
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // EXERCISE 6.7
  // 生成器のListを受け取り、(結果のList, 次の生成器)を返す
  def sequence[A](randList: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      randList.foldRight((List[A](), rng))((rand, acc) => {
        val accRand: RNG = acc._2
        val (a, r) = rand(accRand)

        val accList: List[A] = acc._1

        (a :: accList, r)
      })
    }
  }

  def ints_sequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }

  // EXERCISE 6.8
  // 0 ~ n(nは含まない)までの整数を生成する
  def nonNegativeLessThen(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    val 最大値: Int = n - 1
    val 乱数がnの範囲内でマイナスじゃない = i + 最大値 - mod >= 0
    if (乱数がnの範囲内でマイナスじゃない) (mod, rng2)
    else nonNegativeLessThen(n)(rng2)
  }

  def flatMap[A, B](rand: Rand[A])(f: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r) = rand(rng)
      f(a)(r) // (B, RNG)を返す。
      // f(a)だとRand[B]を返すけど、これだと型が合わない。
      // 「rngをとって(B, RNG)を返す」ではなく「rngをとってRand(rngをとって(B,RNG)を返す)を返す」になってしまうから。
    }
  }

  def nonNegativeLessThen_flatMap(n: Int): Rand[Int] = { rng =>
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      val 最大値: Int = n - 1
      val 乱数がnの範囲内でマイナスじゃない = i + 最大値 - mod >= 0
      if (乱数がnの範囲内でマイナスじゃない) rng => (mod, rng)
      else nonNegativeLessThen(n)
    })(rng)
  }

  // EXERCISE 6.9
  def map_flatMap[A, B](rand: Rand[A])(f: A => B): Rand[B] = {
    flatMap(rand)(i => rng => (f(i), rng))
  }

  def map2_flatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => rng => {
      val (b, r) = rb(rng)
      (f(a, b), r)
    })
  }
}
