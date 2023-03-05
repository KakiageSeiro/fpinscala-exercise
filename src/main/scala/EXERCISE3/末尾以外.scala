package EXERCISE3

// EXERCISE 3.6
object 末尾以外 {
  // 末尾再起じゃないよ。末尾要素以外のlistを引数として渡すようにすれば回避できそう
  // そのリストへの追加を末尾にすると計算量かかりそうだから先頭追加にしておいて、最後にfoldRightとかどうだろうか。
  def init[A](list: List2[A]): List2[A] = list match {
    case Cons(_, Nil) => Nil // 末尾だけ要素を返さない
    case Cons(head, tail) => Cons(head, init(tail))
  }
}