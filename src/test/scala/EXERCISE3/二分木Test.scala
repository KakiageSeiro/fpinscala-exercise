package EXERCISE3

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class 二分木Test extends AnyFlatSpec with Diagrams with TimeLimits {
  "size" should "ノードの数" in {
    assert(二分木.size(二分木.Leaf(1)) == 1)
    assert(二分木.size(二分木.Branch(二分木.Leaf(1), 二分木.Leaf(2))) == 3)
  }
  "sizeViaFold" should "ノードの数" in {
    assert(二分木.sizeViaFold(二分木.Leaf(1)) == 1)
    assert(二分木.sizeViaFold(二分木.Branch(二分木.Leaf(1), 二分木.Leaf(2))) == 3)
  }

  "maximum" should "最大値" in {
    assert(二分木.maximum(二分木.Branch(二分木.Leaf(1), 二分木.Leaf(2))) == 2)
    // こんな感じの木をつくるよ
    //         blanch3
    //    blanch2     l4
    // l3      blanch1
    //      l1        l2
    val l1 = 二分木.Leaf(1)
    val l2 = 二分木.Leaf(2)
    val blanch1 = 二分木.Branch(l1, l2)
    val l3 = 二分木.Leaf(3)
    val blanch2 = 二分木.Branch(l3, blanch1)
    val l4 = 二分木.Leaf(4)
    val blanch3 = 二分木.Branch(blanch2, l4)
    assert(二分木.maximum(blanch3) == 4)
  }

  "depth" should "深さ" in {
    // こんな感じの木をつくるよ
    //         blanch3
    //    blanch2     l4
    // l3      blanch1
    //      l1        l2
    val l1 = 二分木.Leaf(1)
    val l2 = 二分木.Leaf(2)
    val blanch1 = 二分木.Branch(l1, l2)
    val l3 = 二分木.Leaf(3)
    val blanch2 = 二分木.Branch(l3, blanch1)
    val l4 = 二分木.Leaf(4)
    val blanch3 = 二分木.Branch(blanch2, l4)

    assert(二分木.depth(blanch3, 二分木.Leaf(1)) == 3)
    assert(二分木.depth(blanch3, 二分木.Leaf(2)) == 3)
    assert(二分木.depth(blanch3, 二分木.Leaf(3)) == 2)
    assert(二分木.depth(blanch3, 二分木.Leaf(4)) == 1)
  }

  "depth2" should "深さ" in {
    // こんな感じの木をつくるよ
    //         blanch3
    //    blanch2     l4
    // l3      blanch1
    //      l1        l2
    val l1 = 二分木.Leaf(4) // ここを1から4に変えてみた
    val l2 = 二分木.Leaf(2)
    val blanch1 = 二分木.Branch(l1, l2)
    val l3 = 二分木.Leaf(3)
    val blanch2 = 二分木.Branch(l3, blanch1)
    val l4 = 二分木.Leaf(4)
    val blanch3 = 二分木.Branch(blanch2, l4)

    assert(二分木.depth(blanch3, 二分木.Leaf(1)) == -1) // みつからないやつは-1
    assert(二分木.depth(blanch3, 二分木.Leaf(4)) == 3) // 深いほうがヒットする
  }

  // Leafを指定しないで全体の深さをとることに注意
  "depthViaFold" should "深さ" in {
    // こんな感じの木をつくるよ
    //         blanch3
    //    blanch2     l4
    // l3      blanch1
    //      l1        l2
    val l1 = 二分木.Leaf(1)
    val l2 = 二分木.Leaf(2)
    val blanch1 = 二分木.Branch(l1, l2)
    val l3 = 二分木.Leaf(3)
    val blanch2 = 二分木.Branch(l3, blanch1)
    val l4 = 二分木.Leaf(4)
    val blanch3 = 二分木.Branch(blanch2, l4)

    assert(二分木.depthViaFold(blanch3) == 3)
  }

  // Leafを指定しないで全体の深さをとることに注意
  "depthViaFold2" should "深さ" in {
    // こんな感じの木をつくるよ
    //         blanch3
    //    blanch2     l4
    // l3      blanch1
    //      l1        l2
    val l1 = 二分木.Leaf(4) // ここを1から4に変えてみた
    val l2 = 二分木.Leaf(2)
    val blanch1 = 二分木.Branch(l1, l2)
    val l3 = 二分木.Leaf(3)
    val blanch2 = 二分木.Branch(l3, blanch1)
    val l4 = 二分木.Leaf(4)
    val blanch3 = 二分木.Branch(blanch2, l4)

    assert(二分木.depthViaFold(blanch3) == 3)
  }

  "map" should "変換" in {
    // こんな感じの木をつくるよ
    //         blanch3
    //    blanch2     l4
    // l3      blanch1
    //      l1        l2
    val l1 = 二分木.Leaf(1)
    val l2 = 二分木.Leaf(2)
    val blanch1 = 二分木.Branch(l1, l2)
    val l3 = 二分木.Leaf(3)
    val blanch2 = 二分木.Branch(l3, blanch1)
    val l4 = 二分木.Leaf(4)
    val blanch3 = 二分木.Branch(blanch2, l4)

    val 期待値_l1 = 二分木.Leaf(2)
    val 期待値_l2 = 二分木.Leaf(4)
    val 期待値_blanch1 = 二分木.Branch(期待値_l1, 期待値_l2)
    val 期待値_l3 = 二分木.Leaf(6)
    val 期待値_blanch2 = 二分木.Branch(期待値_l3, 期待値_blanch1)
    val 期待値_l4 = 二分木.Leaf(8)
    val 期待値_blanch3 = 二分木.Branch(期待値_blanch2, 期待値_l4)

    assert(二分木.map(blanch3)(a => a + a) == 期待値_blanch3)
  }

  "mapViaFold" should "変換" in {
    // こんな感じの木をつくるよ
    //         blanch3
    //    blanch2     l4
    // l3      blanch1
    //      l1        l2
    val l1 = 二分木.Leaf(1)
    val l2 = 二分木.Leaf(2)
    val blanch1 = 二分木.Branch(l1, l2)
    val l3 = 二分木.Leaf(3)
    val blanch2 = 二分木.Branch(l3, blanch1)
    val l4 = 二分木.Leaf(4)
    val blanch3 = 二分木.Branch(blanch2, l4)

    val 期待値_l1 = 二分木.Leaf(2)
    val 期待値_l2 = 二分木.Leaf(4)
    val 期待値_blanch1 = 二分木.Branch(期待値_l1, 期待値_l2)
    val 期待値_l3 = 二分木.Leaf(6)
    val 期待値_blanch2 = 二分木.Branch(期待値_l3, 期待値_blanch1)
    val 期待値_l4 = 二分木.Leaf(8)
    val 期待値_blanch3 = 二分木.Branch(期待値_blanch2, 期待値_l4)

    assert(二分木.mapViaFold(blanch3)(a => a + a) == 期待値_blanch3)
  }


}
