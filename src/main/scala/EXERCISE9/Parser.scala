package EXERCISE9

// Persersという名前はパーサーが複数ある。という感があるが、そうではなくパーサーを取り巻く要素達って感じ
// [+_]という指定によって
//   _は結果の型を表す型引数を1つ期待していることを示すみたい。後で説明されるとのこと
trait Parsers[ParseError, Parser[+_]] {
  self => // ParsersとParserOpsの両方にor関数を定義するので、Parsersのorを使いたい場合はself.orと書けるように、selfを定義

  // inputはjson文字列
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // 各型に対応するパーサーを作成する関数
  def char(c: Char): Parser[Char]
  // def string(s: String):Parser[String]

  // s1かs2どっちかにマッチすればよいパーサーを作成する関数
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  // Parsers.string("ab")のように書かないとParserにならなかったが"ab"とかいたらこれをParser扱いできるようになる
  // これは↓の中置関数|をつかって"ab" | "bc"のように2つのパーサーを扱う時に見やすくする
  implicit def string(s: String): Parser[String]

  // Parserを使うと、自動的にParserOpsの機能も使えるようにする
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  // ■■■AはすべてStringになる。これは何に使うんだろう？
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  // Parsersをより便利に使うための関数を定義する場所
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
  }
}
