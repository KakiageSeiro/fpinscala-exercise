package EXERCISE12

import EXERCISE11.{Functor, Monad}
import EXERCISE6.State

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  // Monad.stateMonadの型が合わないのでコンパイルエラーになる。どういうことなんだ？
//  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
//    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  // stateのgetとsetとrunができなくてコンパイルエラーになる。どういうことなんだ？
//  def mapAccum[S, A, B](fa: F[A])(f: (A, S) => (B, S)): (F[B], S) =
//    traverseS(fa)((a:A) => (for{
//      s1 <- get[S]
//      (b, s2) = f(a, s1)
//      _ <- set(s2)
//    } yield b)).run(s)

  def fuse[G[_], H[_], A,B](fa:F[A])(f:A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]):(G[F[B]], H[F[B]])=
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)
}

object Traverse {
  // EXERCISE 12.13
//  val listTraverse = new Traverse[List] {
//    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
//      fa.foldRight(G.unit(List[A]()))((acc, a) => G.map2(f(a), fbs)(_ :: _))
//  }
  // 答え見た。↑を書いてたけどGが解決できなかった。
  // そもそもシグネチャからして本に書いてあるのと違うじゃねーか。 implicit G: Applicative[G]なかっただろ
  val listTraverse =
    new Traverse[List] {
      override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
        as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))

      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }

  val optionTraverse =
    new Traverse[Option] {
      override def traverse[G[_], A, B](maybeA: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
        maybeA match {
          case Some(value) => G.map(f(value))(Some(_))
          case None        => G.unit(None)
        }

      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
        case Some(value) => Some(f(value))
        case None        => None
      }
    }

  // EXERCISE12.20
  def composeM[F[_], G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]):Monad[({type f[x] = F[G[x]]})#f] =
    new Monad[({type f[x] = F[G[x]]})#f]{
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

      // 答え見た
//      override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
//        F.flatMap(fga)(ga => G.flatMap(ga)(f)) // 自分で書いたやつ
//        F.flatMap(fga)(ga => G.map(T.traverse(ga)(f))(G.join)) // 答え。暗黙のApplicative[F]がないってコンパイルエラーになる
    }
}
