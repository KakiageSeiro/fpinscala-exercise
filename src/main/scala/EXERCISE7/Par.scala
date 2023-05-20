package EXERCISE7

import java.util.concurrent.{Callable, ExecutorService, Future}
import scala.concurrent.duration.TimeUnit

case class Par[A](run: A){

}

object Par {
  // unitのために必要なエイリアス
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true
    override def get(timeout: Long, units: TimeUnit): A = get
    override def isCancelled: Boolean = false
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
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
}
