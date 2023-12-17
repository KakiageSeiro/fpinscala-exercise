package EXERCISE9

import EXERCISE8.{Gen, Prop}

import scala.language.implicitConversions

// Persersという名前はパーサーが複数ある。という感があるが、そうではなくパーサーを取り巻く要素達って感じ
// [+_]という指定によって
//   _は結果の型を表す型引数を1つ期待していることを示すみたい。後で説明されるとのこと
trait Parsers[ParseError, Parser[+_]] {
  self => // ParsersとParserOpsの両方にor関数を定義するので、Parsersのorを使いたい場合はself.orと書けるように、selfを定義

  // inputはjson文字列
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // 各型に対応するパーサーを作成する関数。ここではcharだけ
  def char(c: Char): Parser[Char] = {
    // stringはOpsの関数でParser[string]を返す。やはりmapをParserクラスのメソッドとして定義することを期待されている？それともこのような使い方をしたいから事前に処理を書いておこうねってことなんだろうか？
    string(c.toString) map (_.charAt(0))
  }

  // s1かs2どっちかにマッチすればよいパーサーを作成する関数
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  // 同じ文字が複数回登場することを確認するパーサー？
  // 期待する動作はこれ
  // run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  // run(listOfN(3, "ab" | "cad"))("cadsbsb") == Right("cadsbsb")
  // run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
  // run(listOfN(3, "ab" | "cad"))("abab") == Left("2回しか登場しないよ")ってこと？
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  // pの値がいくつあるか数える関数だが、なぜか戻り値がParser[List[A]]。countではなくmanyという命名も違和感がないか？
  // そもそもパーサーを受け取ってパーサーを返している。利用用途がイメージできない。runはパースした文字列をそのまま返すことから、count(p: Parser[A]): Intではだめなのか？
  def many[A](p: Parser[A]): Parser[List[A]]

  // manuとあわせてmap(many(char('a')))(_.size)のようにつかうとのこと。
  // mapの結果得られるのはParser[Int]なので「aの個数の数字が含まれる文字列をパースするパーサーを作る」処理になる
  // たとえばmanyの引数にchar('a')を指定したので_.sizeで1になる。Parser[1]になる。このパーサーは1が含まれる文字列をパースする。たとえば"1abc"とか。
  // 用途が意味不明じゃない？mapはともかくmanyは本当に何に使うの？
  //
  // と思ったが、そもそもrunの結果の想定が違いそう
  // val numA: Parser[Int] = char('a').many.map(_.size)
  // run(numA)("aaa")はRight(3)
  // run(numA)("b")はRight(0)
  // のような挙動になるらしい
  // numAのmanyでParser[List[A]]を返すので、それに対してsizeでListのサイズが取れていそう。しかしシグネチャからはParser[size]が返るように見える。
  // 理解できていない部分は
  // ・manyがパースできたcharの数の表現がIntではなくParser[List[A]]であることの意味。これによってmapがわざわざ結果を加工するために生えたように見えている。つまりmanyは後続処理で_.sizeされることを期待していて、そのためにmapが生まれざるを得なかった。
  // ・Parserの型パラメータの意味。Aと同じものが含まれているかを判定するのがParserだと思っていたが、よくわからなくなった。runの実装がまだないからだろうか
  //
  // mapの振舞いは以下の用に期待される
  // map(p)(a => a) == p // 第2引数はaからaへの変換"のみ"していることが前提
  def map[A, B](p: Parser[A])(f: A => B): Parser[B]















  // Lawsは法則の意
  object Laws {
    // forallって何？
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forall(in)(s => run(p1)(s) == run(p2)(s))


    // p.map()という呼出しはできない。map(p)(a => a)にしないと…そもそもParserはまだ定義していないので、classとして定義することを期待されてる？
    // ここでなぜこのような書き方が登場するのかわからない。単に書籍がまちがっているのか、そのように呼び出せるように読者が実装するようにすることを期待されているのか…
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }




  // -------------------------------------------------------------------------------------------------------------------------------------
  // Parsersをより便利に使うための関数を定義する場所
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
  }

  // Parsers.string("ab")のように書かないとParserにならなかったが"ab"とかいたらこれをParser扱いできるようになる
  // これは↓の中置関数|をつかって"ab" | "bc"のように2つのパーサーを扱う時に見やすくする
  implicit def string(s: String): Parser[String]

  // Parserを使うと、自動的にParserOpsの機能も使えるようにする
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  // AをStringのParserにする。Stringなので↑のimplicitによってOpsも使えるようになる。
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

}
