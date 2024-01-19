package EXERCISE11

import EXERCISE8.Gen

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  // EXERCISE 11.8
  // 答え見た。(_: Unit) => maでmaを返すだけの関数を捏造してcomposeの引数の形を捏造してあげればよかったのか
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // EXERCISE 11.13
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(List[A])((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    val b_list = la.foldRight(List[B])((a: A, lb: List[B]) => f(a) :: lb)
    unit(b_list)
  }

  // EXERCISE 11.4
  // モナドを複製の意
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    flatMap(ma)(a => unit(List.fill(n)(a)))
  // sequence(List.fill(n)(ma)) 答えではこうなってた。すでにある道具を使う能力が足りてねえ〜

  // productという名前からはわかりにくいがFでラップしたタプル(A, B)にする君
  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  // EXERCISE 11.6
  // 答え見た。戻り値がList[A]ではなくF[List[A]]であることが理解できなかった。msがF[List[A]]であればわかるが、生Listをfilterするんだったら通常のfilterでいいのでは？
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM(t)(f) // fの結果がfalseだったらhは除外する
        else map(filterM(t)(f))(h :: _))
    }

  // EXERCISE 11.7
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // EXERCISE 11.13
  def compose2[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))

  def flatMap2[A, B](ma: F[A])(f: A => F[B]) =
    join(map(ma)(f))


  // EXERCISE 11.12
  // unitの逆。一番外のFを剥がす
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)
}


object Monad {
  val gemMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit()

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)
  }

  // EXERCISE 11.1
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = Nil

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }
}

// EXERCISE 11.17
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] =
    Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] =
    f(value)
}

object Id {
  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma.value)
  }
}





























