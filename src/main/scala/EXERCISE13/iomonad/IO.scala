package EXERCISE13.iomonad

trait IO { self =>
  def run(): Unit

  // いつもJavaでやってる、クラスのインスタンスを不変にしたいので、変更したい部分だけ受け取って、それ以外はそのままのインスタンスを新たにnewして返すやつ
  def ++(io: IO): IO = new IO {
    def run(): Unit = { self.run(); io.run() } // 自分のrunと引数のrunを連続で実行するよ〜
  }
}

object IO {
  // runに何もないインスタンスを作るやつ。これいらなくね？となるが、これが単位元で++がopのモノイド
  def empty: IO = new IO { def run(): Unit = () }
}
