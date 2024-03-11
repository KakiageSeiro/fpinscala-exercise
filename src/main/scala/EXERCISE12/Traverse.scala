package EXERCISE12

import EXERCISE11.Functor
import EXERCISE3.二分木.Tree

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative2, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative2, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)
}

object Traverse {
  // EXERCISE 12.13
//  val listTraverse = new Traverse[List] {
//    override def traverse[G[_] : Applicative2, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
//      fa.foldRight(G.unit(List[A]()))((acc, a) => G.map2(f(a), fbs)(_ :: _))
//  }
  // 答え見た。↑を書いてたけどGが解決できなかった。
  // そもそもシグネチャからして本に書いてあるのと違うじゃねーか。 implicit G: Applicative[G]なかっただろ
  val listTraverse =
    new Traverse[List] {
      override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative2[G]): G[List[B]] =
        as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))

      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }

  val optionTraverse =
    new Traverse[Option] {
      override def traverse[G[_], A, B](maybeA: Option[A])(f: A => G[B])(implicit G: Applicative2[G]): G[Option[B]] = maybeA match {
          case Some(value) => G.map(f(value))(Some(_))
          case None => G.unit(None)
        }

      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
          case Some(value) => Some(f(value))
          case None => None
        }
    }
}
