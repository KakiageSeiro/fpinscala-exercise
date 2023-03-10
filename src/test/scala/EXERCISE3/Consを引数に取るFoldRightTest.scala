package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class Consを引数に取るFoldRightTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "Consを引数に取るFoldRight" should "できる" in {
    val 期待値 = Cons(1, Cons(2, Cons(3, Nil)))
    assert(Consを引数に取るFoldRight.foldRight(List2(1, 2, 3), Nil:List2[Int])(Cons(_,_)) == 期待値)
    // List2(1, 2, 3) は Cons(1, Cons(2, Cons(3, Nil))) と同じになる
    // これはFoldRightでネストした構造の一番奥から処理が開始され、そこでConsを作るため。
    // ConsはList構造の表現にheadとtail(List2traitを実装したCons)を持つネストした構造なので、以下のようにConsのコンストラクタが呼ばれる。
    // head=3, tail=Nil
    // head=2, tail=Cons(3, Nil)
    // head=1, tail=Cons(2, Cons(3, Nil))
    // Cons(1, Cons(2, Cons(3, Nil)))
    // Consのデータの持ち方と、FoldRightは相性が良いといえる。
  }
}
