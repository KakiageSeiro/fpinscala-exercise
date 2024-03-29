package EXERCISE13.iomonad

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

object IO {
  // runに何もないインスタンスを作るやつ。これいらなくね？となるが、これが単位元で++がopのモノイド
//  def empty: IO = new IO { def run(): Unit = () }
}
