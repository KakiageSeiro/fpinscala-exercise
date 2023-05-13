package EXERCISE6

import EXERCISE6.State.{get, modify, sequence}

// EXERCISE 6.10
// 一般化せよの意味が分からなくて答え見た。状態は今までランダムな何かを生成するRNGをサンプルとして使っていたが、それを型パラメータSとしてつかうStateを作れと言うことみたい。
case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      }
    )

  // mapはflatMapに渡す関数のA部分をfで変換するだけ
  def map[B](f: A => B): State[S, B] = flatMap(a => State(s => (f(a), s))) // ここはState.unit(f(a))をつかっても

  // map2の引数fはABをとるので、レシーバーと引数両方を渡す。
  // flatMapを使うことでレシーバーを引き出す役、mapは引数を引き出す役
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))

    // ○○は引き出す役。の用な場合はfor式を使うとわかりやすい。↑のコメントで書いてあることが直感的。
    //    for {
    //      a <- this
    //      b <- sb
    //    } yield f(a, b)
  }
}


object State {
  // aを返すStateを作る
  // Stateを作る時は関数runを定義するが、それを書くのが面倒くさい用？
  // Scalaを書く人たちも関数の定義がたくさんあると目が滑るのかな
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](stateList: List[State[S, A]]): State[S, List[A]] = {
    val z: State[S, List[A]] = unit[S, List[A]](List())
    stateList.foldRight(z)((state, acc) => state.map2(acc)(_ :: _))
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // fによって現在の状態を書き換え
  // ここのyieldと戻り値のState[S, Unit]がよく分からない
  // yieldで()なのに戻り値がある？
  // とおもったけどsetの結果がそのまま返ってるみたい。
  // 状態遷移させたい時は、状態を返す関数をこれに渡す
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

// (キャンディの)自動販売機
sealed trait Input // インプットというよりは操作(イベントでも良いかも)

case object Coin extends Input // コイン

case object Turn extends Input // ハンドルを回す

case object Lever extends Input // コイン返却レバーを回す

// locked=falseならハンドルを回せる
// currentCoinsはキャンディをもらってない状態で投入されたコインの数。レバーを引くとこれが返却される。
case class Machine(locked: Boolean, キャンディ: Int, coins: Int, currentCoins: Int)

object 自動販売機 {
  // State[Machine, (Int, Int)]のInt一つ目はコインの数、二つ目はキャンディの数
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(input => modify(update(input))))
    s <- get
  } yield (s.coins, s.キャンディ)

  def update: Input => Machine => Machine =
    (input: Input) => (s_状態: Machine) =>
      (input, s_状態) match {
        case (_, Machine(_, 0, _, _)) => s_状態 // キャンディがない場合は何もしない
        case (Coin, Machine(false, _, _, _)) => s_状態 // ハンドルを回せる状態でコインを入れても何もしない
        case (Turn, Machine(true, _, _, _)) => s_状態 // ロック状態ではハンドルを回せない
        case (Coin, Machine(true, candy, coin, currentCoins)) => // ロック状態でコインを入れたらロック解除
          Machine(false, candy, coin + 1, currentCoins + 1)
        case (Turn, Machine(false, candy, coin, _)) => // ロック解除状態でハンドルを回したらキャンディを出す
          Machine(true, candy - 1, coin, 0)
        case (Lever, Machine(true, _, _, _)) => s_状態 // ロック状態でコイン返却レバー回しても意味ない
        case (Lever, Machine(false, candy, coin, currentCoins)) => // ロック解除状態でコイン返却レバー回したらコインを返却
          Machine(true, candy, coin - currentCoins, 0)
      }
}
