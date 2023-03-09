import EXERCISE3.List2
import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class 型推論Test extends AnyFlatSpec with Diagrams with TimeLimits {
  "ふつうに" should "よびだし" in {
    assert(型推論.dropWhile(List2(1, 2, 3, 4), (a: Int) => a < 3) == List2(3, 4))
  }

  "fの型指定しないで" should "エラー" in {
    // コンパイル時にエラー(missing parameter type)になる
    // assert(型推論.dropWhile(List2(1, 2, 3, 4), (a) => a < 3) == List2(3, 4))
  }

  "引数逆" should "よびだし" in {
    assert(型推論.dropWhileReverse((a: Int) => a < 3, List2(1, 2, 3, 4)) == List2(3, 4))
  }

  "引数逆_fの型指定しないで" should "よびだし" in {
    // コンパイル時にエラー(missing parameter type)になる
    assert(型推論.dropWhileReverse((a) => a < 3, List2(1, 2, 3, 4)) == List2(3, 4))
  }

  "カリー化した" should "型推論できるので通る" in {
    assert(型推論.dropWhileCurry(List2(1, 2, 3, 4))(a => a < 3) == List2(3, 4))
  }

  "カリー化した引数逆" should "エラー" in {
    // コンパイル時にエラー(missing parameter type)になる
    // assert(型推論.dropWhile3(a => a < 3)(List2(1, 2, 3, 4)) == List2(3, 4))
  }

  // このことから、カリー化した関数で、型パラメータを取る型は引数の左側に書くと、右の引数では推論できる。

}
