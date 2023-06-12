package EXERCISE7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  // unitのために必要なエイリアス
  // 型エイリアスでは中置構文を使えない。使えるようにしたい時は暗黙の型変換(implicit conversion)を使う。
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true

    override def get(timeout: Long, units: TimeUnit): A = get

    override def isCancelled: Boolean = false

    // cancel関数を呼ぶのに引数でキャンセルしないというのはどういう用途？
    override def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // この実装の問題
  // 1. Callableのcall実行中はブロックする
  // 2. es.submitとa(es)で2つのスレッドを使う。callでは1つの処理しかしてないので、そのスレッドでやればよい。
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  // fは本スレッドでやる。別スレッドでやりたい時はfork(Par.map2(a, b)(f))のようにforkで包む
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(sum(l), sum(r))(_ + _)
    }

  // EXERCISE 7.3
  def map2Timeouts[A, B, C](parA: ExecutorService => Future[A], parB: ExecutorService => Future[B])(abからcにするf: (A, B) => C): ExecutorService => Future[C] = {
    es: ExecutorService =>
      new Future[C] {
        private val futureA: Future[A] = parA(es)
        private val futureB: Future[B] = parB(es)
        // @volatileによって、マシン語レベルでキャッシュされることによりスレッドごとに違う値になってしまう現象を防ぐことができるみたい。理解が浅いので注意。
        @volatile private var cache: Option[C] = None

        def isDone: Boolean = cache.isDefined

        // 実質タイムアウトなしになるでかい時間指定
        def get(): C = get(Long.MaxValue, TimeUnit.NANOSECONDS)

        def get(timeout: Long, units: TimeUnit): C = {
          val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, units)
          val 開始時間 = System.nanoTime
          val a = futureA.get(timeoutNanos, TimeUnit.NANOSECONDS)
          val 経過時間 = System.nanoTime - 開始時間
          val b = futureB.get(timeoutNanos - 経過時間, TimeUnit.NANOSECONDS)
          val c = abからcにするf(a, b)
          cache = Some(c)
          c
        }

        def isCancelled: Boolean = futureA.isCancelled || futureB.isCancelled

        def cancel(evenIfRunning: Boolean): Boolean =
          futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)
      }
  }

  // EXERCISE 7.4
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  // EXERCISE 7.5
  def sequence[A](list: List[Par[A]]): Par[List[A]] =
    list.foldRight(unit(List.empty[A]))((par, acc) => map2(par, acc)(_ :: _))

  // 答え見た。foldRightの分割統治方の分割部分を別の計算リソースに切り出せるときはlogNの形に出来る
  def sequenceBalanced[A](parSeq: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = parSeq match {
    case IndexedSeq() => unit(IndexedSeq.empty[A])
    case IndexedSeq(par) if parSeq.size == 1 => map(par)(a => IndexedSeq(a))
    case _ =>
      val (l, r) = parSeq.splitAt(parSeq.size / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
  }

  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(pas.toIndexedSeq))(_.toList)

  def map[A, B](par: Par[A])(f: A => B): Par[B] = {
    // unit(())は利用しない
    map2(par, unit(()))((a, _) => f(a))
  }

  // mapしたあとParで包むよ。並列で計算できるので速い(はず)
  def parMap[A, B](aList: List[A])(f: A => B): Par[List[B]] = fork {
    val bList: List[Par[B]] = aList.map(asyncF(f))
    sequence(bList)
  }

  // EXERCISE 7.6
  def parFilter[A](aList: List[A])(f: A => Boolean): Par[List[A]] = {
    val filter: A => Par[List[A]] =
      asyncF((a: A) => if (f(a)) List(a) else List.empty[A])

    sequence(aList.map(filter))
  }
}

