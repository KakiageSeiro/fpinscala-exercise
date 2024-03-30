package EXERCISE13.iomonad

import EXERCISE12.Monad

import scala.io.StdIn.readLine

trait IO[A] { self =>
  def run(): A

  // いつもJavaでやってる、クラスのインスタンスを不変にしたいので、変更したい部分だけ受け取って、それ以外はそのままのインスタンスを新たにnewして返すやつ
//  def ++(io: IO): IO = new IO {
//    def run(): Unit = { self.run(); io.run() } // 自分のrunと引数のrunを連続で実行するよ〜
//  }

  def map[B](f: A => B): IO[B] = new IO[B] {
    // トレイトの型パラメータをBでnewしているので、run()もAではなくBを返すようになったよ
    def run(): B = f(self.run())
  }

  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    // トレイトの型パラメータをBでnewしているので、run()もAではなくBを返すようになったよ
    def run(): B = f(self.run()).run()
  }
}

object IO extends Monad[IO] {
  // ScalaFmtでフォーマットするとflatMap,apply,unitの順序に整形される。順序は変えないようにしたいけど公式ドキュメントに書いて無くね？

  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

  // このメソッドによって、IO{...}のようにIOブロックを生成する関数適用構文を使用できるようになる
  def apply[A](a: => A): IO[A] = unit(a)

  override def unit[A](a: => A): IO[A] = new IO[A] {
    def run(): A = a
  }

  // runに何もないインスタンスを作るやつ。これいらなくね？となるが、これが単位元で++がopのモノイド
  //  def empty: IO = new IO { def run(): Unit = () }

  // 10行読み取ってListにして返す。inputの副作用があるのでIOで包む
  // replicateMの使い方があんまりピンと来てなかったけど、これは例としてわかりやすい。
  // シグネチャはこれ def replicateM[A](n: Int)(fa: F[A]): F[List[A]]
  // faを10回やるけど、どうせ10回セットでやるのでIOで包むのは一番外にして(最後にsequenceやる感じ)10回やった結果はListにしている。と考えるとスッキリ
  def replicate10: IO[List[String]] = replicateM(10)(ReadLine)
}

def ReadLine: IO[String] = IO {
  readLine
}

def PrintLine(msg: String): IO[Unit] = IO {
  println(msg)
}

// UnitではなくIO[Unit]になった。IOはMonadなので合成できるし呼出しタイミングではなくrunしたタイミングで発火する
def converter: IO[Unit] = for {
  _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ") // 温度を華氏で入力してね
  d <- ReadLine.map(_.toDouble)
  _ <- PrintLine(fahrenheitToCelsius(d).toString)
} yield ()

// 華氏(°F)から摂氏(℃)へ
def fahrenheitToCelsius(f: Double): Double =
  (f - 32) * 5.0 / 9.0


// 階乗。定義していない関数が多数登場するが、あくまでも例なので…ということらしい
// ミュータブルな参照を触って書き換えることができるよ。それをしてもIOが戻り値なので関数型プログラミングを邪魔しないよ。という例
//
//def factorial(n: Int): IO[Int] = for {
//  acc <- ref(1)
//  _ <- foreachM (1 to n toStream) (i => acc.modify(_ * i).skip)
//  result <- acc.get
//} yield result

// こちらは対話型のUIを実現するのに、無限ループとかを使ってるけど、IOが戻り値なので関数型プログラミングを邪魔しないよ。の例
//
//val factorialREPL: IO[Unit] = sequence_(
//  IO { println(helpstring) },
//  doWhile { IO { readLine } } { line =>
//    val ok = line != "q"
//    when (ok) { for {
//      n <- factorial(line.toInt)
//      _ <- IO { println("factorial: " + n) }
//    } yield () }
//  }
//)







