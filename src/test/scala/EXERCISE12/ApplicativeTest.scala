package EXERCISE12

import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class ApplicativeTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "エラーにならない" should "Successが返ってくる" in {
    // テスト用のデータ
    val validData = ("John Doe", "1990-01-01", "123-456-7890")

    val result: Validation[String, validationApplicative.WebForm] =
      validationApplicative.validWebForm(
        validData._1,
        validData._2,
        validData._3
      )
    result match {
      case Success(r) =>
        assert(r.name == validData._1)
        assert(r.birthdate == validData._2)
        assert(r.phone == validData._3)
      case Failure(h, t) => fail("成功するはずがFailure返ってきた")
    }
  }

  "名前エラーが" should "返ってくるよ" in {
    // テスト用のデータ
    val invalidName = ("", "1990-01-01", "123-456-7890")

    val result = validationApplicative.validWebForm(
      invalidName._1,
      invalidName._2,
      invalidName._3
    )
    result match {
      case Success(r) => fail("失敗するはずがSuccess返ってきた")
      case Failure(h, t) =>
        assert(h == "名前が入力されてないよ〜")
        assert(t.isEmpty)
    }

  }

  "誕生日エラーが" should "返ってくるよ" in {
    // テスト用のデータ
    val invalidBirthdate = ("John Doe", "", "123-456-7890")

    val result = validationApplicative.validWebForm(
      invalidBirthdate._1,
      invalidBirthdate._2,
      invalidBirthdate._3
    )
    result match {
      case Success(r) => fail("失敗するはずがSuccess返ってきた")
      case Failure(h, t) =>
        assert(h == "誕生日がにゅうりょくされてないよ！")
        assert(t.isEmpty)
    }
  }

  "電話番号エラーが" should "返ってくるよ" in {
    // テスト用のデータ
    val invalidPhone = ("John Doe", "1990-01-01", "")

    val result = validationApplicative.validWebForm(
      invalidPhone._1,
      invalidPhone._2,
      invalidPhone._3
    )
    result match {
      case Success(r) => fail("失敗するはずがSuccess返ってきた")
      case Failure(h, t) =>
        assert(h == "電話番号が入力されてないよ〜")
        assert(t.isEmpty)
    }
  }

  "複数エラーが" should "返ってくるよ" in {
    // テスト用のデータ
    val invalidAll = ("", "", "")

    val result = validationApplicative.validWebForm(
      invalidAll._1,
      invalidAll._2,
      invalidAll._3
    )
    result match {
      case Success(r) => fail("失敗するはずがSuccess返ってきた")
      case Failure(h, t) =>
        assert(h == "名前が入力されてないよ〜")
        assert(t.size == 2)
        assert(t(0) == "誕生日がにゅうりょくされてないよ！")
        assert(t(1) == "電話番号が入力されてないよ〜")
    }
  }
}
