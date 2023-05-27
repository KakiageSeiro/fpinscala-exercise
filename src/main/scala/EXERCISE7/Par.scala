package EXERCISE7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

case class Par[A](run: A) {}

object Par {
  // unitのために必要なエイリアス
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
}

