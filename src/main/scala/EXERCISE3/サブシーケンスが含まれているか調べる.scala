package EXERCISE3

import scala.annotation.tailrec

object サブシーケンスが含まれているか調べる {
  // EXERCISE 3.24
  @tailrec
  def hasSubsequence[A](sup: List2[A], sub: List2[A]): Boolean = sup match {
    case Nil => sub == Nil
    case Cons(h,t) if existsSub(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub) // 次の文字
  }

  def existsSub[A](sup: A, sub: List2[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true // subの最後まで一致した
    case (Cons(supHead,supTail), Cons(subHead,subTail)) if supHead == subHead => existsSub(supTail, subTail)
    case _ => false // 一致しなかった
  }
}