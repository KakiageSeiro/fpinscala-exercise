package EXERCISE11

trait Functor[F[_]] {
  // モナドF？の中の値に関数を適用するやつ
  def map[A, B](fa: F[A])(f: A => B): F[B]

  // F[(A, B)]は(f[A], f[B])に分解できる
  // distributeはディストリビュート(Linuxのディストリビューションの単語っぽい)とよみ、分配という意味みたい
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  // この処理を見ると、Eitherはエラーを左右で表す型というよりは、三項演算子のように左がfalseなら右を適用する型と見なせる？
  // codistributeは共配布という意味みたい。謎
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_)) // fa: F[A]の中の値であるAをLeftで包む
    case Right(fb) => map(fb)(Right(_)) // fb: F[B]の中の値であるBをRightで包む
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}
