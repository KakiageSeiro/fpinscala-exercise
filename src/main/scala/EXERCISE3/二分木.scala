package EXERCISE3

object 二分木 {
  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // EXERCISE 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // EXERCISE 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  // EXERCISE 3.27
  def depth[T](tree: Tree[T], target: Leaf[T]): Int = {
    def go(tree: Tree[T], currentDepth: Int): Int = tree match {
      case Leaf(x) if Leaf(x) == target => currentDepth
      case Branch(l, r) => go(l, currentDepth + 1).max(go(r, currentDepth + 1))
      case _ => -1 // みつからなかったら-1返しとけ！
    }

    go(tree, 0)
  }

  // EXERCISE 3.28
  def map[A](tree: Tree[A])(f: A => A): Tree[A] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch[A](map(l)(f), map(r)(f))
  }

  // EXERCISE 3.29 わからなかったので答えを見た
  // https://github.com/fpinscala/fpinscala/blob/e663f88c88fcca9b20743b071e2d22923bb0a2bf/answerkey/datastructures/29.answer.md
  private def fold[A, B](tree: Tree[A])(leaf用f: A => B, branch用f: (B, B) => B): B = tree match {
    case Leaf(a) => leaf用f(a)
    // この発想がなかった。sizeでInt、mapでTree[A]の2つの型を記述することが出来ないと思い解けなかった
    // Branchのcaseでやってることが「l,rに分ける」「l,rの結果をまとめる」であることに分割して考えればよかったかも。そうすれば後者がbranch用fとして必要である発想になったかも。
    // 「l,rの結果をまとめる」を「sizeの場合はInt, mapの場合はTreeを返すなにか」と考えたのはよくなかった。
    case Branch(l, r) => branch用f(
      fold(l)(leaf用f, branch用f),
      fold(r)(leaf用f, branch用f)
    )
  }

  def sizeViaFold[A](tree: Tree[A]): Int = {
    val leaf用f = (a: A) => 1
    val branch用f = (b1: Int, b2: Int) => (1 + b1 + b2)
    fold(tree)(leaf用f, branch用f)
  }

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)((a: A) => 0, (d1: Int, d2: Int) => 1 + (d1 max d2))

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    val leaf用f = (a: A) => Leaf(f(a))
    val branch用f = (b1: Tree[B], b2: Tree[B]) => Branch(b1, b2)
    fold(tree)(leaf用f, branch用f)
  }
}