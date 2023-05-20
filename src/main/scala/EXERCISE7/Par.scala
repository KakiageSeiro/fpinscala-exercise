package EXERCISE7

case class Par[A](run: A){

}

object Par {
  // 最初のsum
//  def sum(ints: IndexedSeq[Int]): Int =
//    if (ints.size <= 1)
//      ints.headOption getOrElse 0
//    else {
//      val (l, r) = ints.splitAt(ints.length / 2)
//      sum(l) + sum(r)
//    }

  def unit[A](a: => A): Par[A] = Par(a)
  def get[A](a: Par[A]): A = a.run

  // EXERCISE7.1
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    unit(f(a.run, b.run))

  // mat2を使うsum
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
}
