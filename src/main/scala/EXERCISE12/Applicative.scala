package EXERCISE12

import EXERCISE11.Functor

// unitとapplyのプリミティブでも、map2とunitと同じことができる
trait Applicative2[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  // 単純に map2 を使って関数を F にリフティングし、それを fab と fa の両方に適用できるようにする。
  // fab と fa の両方に適用できる。ここでリフティングされる関数は _(_) である、
  // これはラムダ記法 (f, x) => f(x) と同じである。つまり
  // これは2つの引数を取る関数である：
  // 1. 関数 f
  // 2. その関数に対する引数 x
  // そして、単純に f を x に適用する
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fab, fa)(_(_))
    // 第二引数の_(_)は
    // 最初の_には、fabの文脈を外したA => B
    // 二つ目の_には、faの文脈を外したAが入ってくる
    // 文脈外しはmap2の実装でmapを使っていることで実現できている
  }

  // EXERCISE 12.2
  // 答え見た。言ってることはわかるがややこし〜〜〜。以下は翻訳
  // map2 はまず f を curry することで実装される。
  // これは A を受け取って B => C 型の別の関数を返す関数である。
  // この関数を apply に渡すと F[B => C] が得られる。
  // F[B] と一緒に apply に渡すと、目的の F[C] が得られる。
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val a_bc: A => B => C = f.curried
    val fbc: F[B => C] = map(fa)(a_bc)
    apply(fbc)(fb)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  // EXERCISE 12.3
  // 答え見た。curriedがうまく使えていないのでこの発想がでてこないのでは？
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(
    f: (A, B, C) => D
  ): F[D] = {
    val curried: A => B => C => D = f.curried
    apply(apply(apply(unit(curried))(fa))(fb))(fc)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
    f: (A, B, C, D) => E
  ): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

}

trait Applicative[F[_]] extends Functor[F] {
  // プリミティブコンビネータ
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  // 派生コンビネータ
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    map2(fa, unit(()))((a, _) => f(a))
    // unit(())という呼び出し方はnew ArrayList<String>()みたいなもん。ガワだけ作るぞ
  }

  // AのListをBのListに変換したあとにFで包むよっていうのがtraverse(縦走という意味らしい)なのかも
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    // aをF[B]に変換して
    // fbs:F[List[B]] に追加するよ
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
  }

  // EXERCISE 12.1

  // sequence(シーケンス)は複数のファンクタ付きのAを、まとめる処理っぽい。外側を入れ替えると覚えたほうが良いかも
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  // replicateM(M(モナド)をレプリカする)はfaの複製をして、ファンクタは個々の要素ではなく全体で一つで良いのでsequenceでまとめる処理っぽい
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // product(直積)はファンクタの型パラメータのペアをつくり、またファンクタで包む処理っぽい
  // F がOptionモナドであれば、2つのOption値を組み合わせて(A, B)のタプルを生成する
  // もしfaがSome(1)でfbがSome("hello")の場合、Some((1, "hello")) になる
  // fa,fbのどちらかがNoneであれば結果はNoneになる
  // A,Bの型がちがくてもOKなのがポイント。直積の操作はファンクタによって実現しているので、中身は関係ない
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

}

trait Monad[F[_]] extends Applicative2[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  // 外側を外すだけ
  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

// EXERCISE 12.5
// for {
//   f1 <- validName(field1)
//   f2 <- validBirthdate(field2)
//   f3 <- validPhone(field3)
// } yield WebForm(f1, f2, f3)
// のように利用することを考える。validNameが失敗した場合はvalidBirthdateは実行されない
//
// 次に、map3で上記3つのvalidを実行することを考える。
// map3(
//   validName(field1)
//   validBirthdate(field2)
//   validPhone(field3)
// )(WebForm(f1, f2, f3))
// これは3つのvalidの間に依存が暗示されていない
// が、Eitherモナドではmap3はflatMapを利用して実装されている
// (実装見てないけど、そういう挙動をするのはわかるよな)ので
// 最初にエラーが発生したタイミングで止まる(そしてエラーを1つだけ返す)
// だが、バリデーションのようなケースだとすべてのフィールドのエラーを全部検証したあとにエラーをまとめてほしい
//def eitherMonad[Error]: Monad[({type f[x] = Either[Error, x]})#f] =
//  new Monad[({type f[x] = Either[Error, x]})#f] {
//
//    def unit[A](a: => A): Either[Error, A] = Right(a)
//
//    override def flatMap[A,B](eea: Either[Error, A])(f: A => Either[Error, B]) = eea match {
//      case Right(a) => f(a)
//      case Left(e) => Left(e)
//    }
//  }
//
// 上記Eitherモナドの問題点を解決するtraitを考える
sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]


object validationApplicative {
  // EXERCISE 12.6
  def validationApplicative[E]: Applicative2[({type f[x] = Validation[E, x]})#f] =
    new Applicative2[({type f[x] = Validation[E, x]})#f] {

      def unit[A](a: => A) = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C) =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))

          // 全部のエラーを連結する。ここでEitherモナドの問題を解決している
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)

          // どちらかがFailureのパターンは、一つのFailureだけ返す
          case (e@Failure(_, _), _) => e // e@Failure(_, _)という記法は、eという変数にFailureインスタンスをバインドする記法
          case (_, e@Failure(_, _)) => e
        }

      override def map3[A, B, C, D](fa: Validation[E, A], fb: Validation[E, B], fc: Validation[E, C])(f: (A, B, C) => D): Validation[E, D] =
        (fa, fb, fc) match {
          case (Success(a), Success(b), Success(c)) => Success(f(a, b, c))

          // 全部のエラーを連結する。ここでEitherモナドの問題を解決している
          case (Failure(h1, t1), Failure(h2, t2), Failure(h3, t3)) => Failure(h1, t1 ++ Vector(h2, h3) ++ t2 ++ t3)

          // どれかがFailureのパターンは、一つのFailureだけ返す
          case (e@Failure(_, _), _, _) => e // e@Failure(_, _)という記法は、eという変数にFailureインスタンスをバインドする記法
          case (_, e@Failure(_, _), _) => e
          case (_, _, e@Failure(_, _)) => e
        }

    }

  // valid○○の関数からはValidation型を返すようにする
  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("名前が入力されてないよ〜")

  // 誕生日と電話番号の実装は適当
  def validBirthdate(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("誕生日がにゅうりょくされてないよ！")

  def validPhone(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("電話番号が入力されてないよ〜")

  case class WebForm(name: String, birthdate: String, phone: String)

  def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
    validationApplicative.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone)
    )(WebForm)
}








